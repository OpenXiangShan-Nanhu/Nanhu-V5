package xiangshan.cache.sram

import utils._
import utility._
import chisel3._
import chisel3.util._
import chisel3.experimental.{requireIsChiselType, Direction}
import org.chipsalliance.cde.config.Parameters
import utility.mbist.MbistPipeline

/** A hardware module implementing a Queue using SRAM, it consists of a register and SIZE-1 SRAM
  * @param gen The type of data to queue
  * @param entries The max number of entries in the queue
  * @param pipe True if a single entry queue can run at full throughput (like a pipeline). The ''ready'' signals are
  * combinationally coupled.
  * @param flow True if the inputs can be consumed on the same cycle (the inputs "flow" through the queue immediately).
  * The ''valid'' signals are coupled.
  * @param hasFlush True if generated queue requires a flush feature
  * @example {{{
  * val q = Module(new Queue(UInt(), 16))
  * q.io.enq <> producer.io.out
  * consumer.io.in <> q.io.deq
  * }}}
  */
class SRAMQueue[T <: Data](
  val gen:      T,
  val entries:  Int,
  val pipe:     Boolean = false,
  val flow:     Boolean = false,
  val hasFlush: Boolean = false,
  // use in sram
  val singlePort: Boolean = false,
  val hasMbist:   Boolean = false
)(
  implicit p: Parameters)
    extends Module() {
  require(entries > -1, "Queue must have non-negative number of entries")
  require(entries != 0, "Use companion object Queue.apply for zero entries")
  require(entries > 1, "SRAMQueue has one entrie that is a register, so the size must be greater than 1")
  requireIsChiselType(gen)

  // io
  val io = IO(new QueueIO(gen, entries, hasFlush))

  // sram
  val sram = Module(
    new SRAMTemplate(
      gen,
      entries - 1,
      singlePort = singlePort,
      bypassWrite = !singlePort,
      shouldReset = true,
      hasMbist = hasMbist
    )
  )
  val mbistPl = MbistPipeline.PlaceMbistPipeline(1, place = hasMbist)

  // when deq ready false, store sram data temporarily in a register.
  val temp_valid = RegInit(false.B)
  val temp = RegInit(0.U.asTypeOf(gen))

  // sram ptr
  val sram_enq_ptr = Counter(entries - 1)
  val sram_deq_ptr = Counter(entries - 1)
  val sram_maybe_full = RegInit(false.B)
  val sram_ptr_match = sram_enq_ptr.value === sram_deq_ptr.value
  val sram_empty = sram_ptr_match && !sram_maybe_full
  val sram_full = sram_ptr_match && sram_maybe_full

  // enq and deq logic
  val sram2temp = RegInit(false.B)
  val do_flow_false = WireInit(true.B)
  val do_flow = WireInit(false.B)
  val do_temp = WireInit(io.enq.fire & sram_empty & !temp_valid & !sram2temp)
  val sram_do_enq = WireDefault(io.enq.fire & !do_temp)
  val sram_do_deq = WireDefault(io.deq.fire & (!sram_empty | sram_do_enq))
  sram2temp := sram_do_deq // data read from sram, data will be storing in temp in next cycle

  if (flow) {
    do_flow := io.enq.fire & !temp_valid & !sram2temp
    do_flow_false := do_flow & !io.deq.ready
    do_temp := io.enq.fire & sram_empty & !temp_valid & !sram2temp & do_flow_false
    sram_do_enq := io.enq.fire & !do_temp & !do_flow
  }

  if (singlePort) {
    do_flow := io.enq.fire & !temp_valid & !sram2temp & sram_empty
    sram_do_deq := (!(temp_valid | sram2temp) | io.deq.fire) & !sram_empty & !sram_do_enq
  } else {
    when(sram_do_enq | sram_do_deq | !sram_empty) { assert(temp_valid | sram2temp) }
  }

  // flush
  val flush = io.flush.getOrElse(false.B)

  dontTouch(do_temp)
  dontTouch(do_flow)
  dontTouch(sram_do_enq)
  dontTouch(sram_do_deq)

  // sram ptr ctrl logic
  when(sram_do_enq) {
    sram_enq_ptr.inc()
  }
  when(sram_do_deq) {
    sram_deq_ptr.inc()
  }
  when(sram_do_enq =/= sram_do_deq) {
    sram_maybe_full := sram_do_enq
  }

  // when flush is high, empty the queue
  // Semantically, any enqueues happen before the flush.
  when(flush) {
    sram_enq_ptr.reset()
    sram_deq_ptr.reset()
    sram_maybe_full := false.B
    temp_valid := false.B
    sram2temp := false.B
  }

  // io valid and ready logic
  io.deq.valid := do_flow | temp_valid | sram2temp
  if (!pipe) { io.enq.ready := !sram_full }
  else { io.enq.ready := !sram_full | io.deq.ready }

  // sram write
  sram.io.w(sram_do_enq, io.enq.bits, sram_enq_ptr.value, 1.U)

  // sram read
  val sram_resp = sram.io.r(sram_do_deq, sram_deq_ptr.value).resp.data(0)
  dontTouch(sram_resp)

  // temp
  when(io.deq.fire) { temp_valid := false.B }
  when(do_temp) {
    temp := io.enq.bits
    temp_valid := true.B
  }.elsewhen(sram2temp & !io.deq.fire) { // io.deq.fire indicates that data has been exported in sram2temp
    temp := sram_resp
  }
  when(sram2temp & !io.deq.fire) { temp_valid := true.B }

  // io.deq.bits
  io.deq.bits := Mux(sram2temp, sram_resp, Mux(do_flow, io.enq.bits, temp))
  assert(
    (do_flow.asUInt + temp_valid.asUInt + sram2temp.asUInt) <= 1.U,
    "Queue should be no more than one output in one cycle"
  )

  // count entries
  val sram_count = WireInit(0.U)
  sram_count := Mux(
    sram_ptr_match,
    Mux(sram_maybe_full, entries.asUInt - 1.U, 0.U),
    Mux(
      sram_deq_ptr.value > sram_enq_ptr.value,
      entries.asUInt - 1.U - sram_deq_ptr.value + sram_enq_ptr.value,
      sram_enq_ptr.value - sram_deq_ptr.value
    )
  )
  dontTouch(sram_count)
  io.count := sram_count + temp_valid.asUInt + sram2temp.asUInt

}