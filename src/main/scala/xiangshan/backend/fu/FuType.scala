package xiangshan.backend.fu

import chisel3._
import chisel3.util._
import chisel3.util.BitPat
import utils.EnumUtils.OHEnumeration
import org.chipsalliance.cde.config.Parameters
import xiangshan.XSCoreParamsKey

import scala.language.implicitConversions

object FuType extends Enumeration {
  // class OHType(i: Int, name: String) extends super.OHVal(i: Int, name: String)

  // def OHType(i: Int, name: String): OHType = new OHType(i, name)

  // implicit class fromOHValToLiteral(x: OHType) {
  //   def U: UInt = x.ohid.U
  //   def U(width: Width): UInt = x.ohid.U(width)
  // }

  private var initVal = 0

  // private def Value(name: String): OHType = {
  //   val ohval = OHType(initVal, name)
  //   initVal += 1
  //   ohval
  // }

  // int
  val jmp = Value("jmp")
  val brh = Value("brh")
  val i2f = Value("i2f")
  val i2v = Value("i2v")
  val f2v = Value("f2v")
  val csr = Value("csr")
  val alu = Value("alu")
  val mul = Value("mul")
  val div = Value("div")
  val fence = Value("fence")
  val bku = Value("bku")

  // ls
  val ldu = Value("ldu")
  val stu = Value("stu")
  val mou = Value("mou")

  // vec
  val vipu = Value("vipu")
  val vialuF = Value("vialuF")
  val vppu = Value("vppu")
  val vimac = Value("vimac")
  val vidiv = Value("vidiv")
  val vfalu = Value("vfalu")
  val vfma = Value("vfma")
  val vfdiv = Value("vfdiv")
  val vfcvt = Value("vfcvt")
  val vsetiwi = Value("vsetiwi") // vset read rs write rd
  val vsetiwf = Value("vsetiwf") // vset read rs write vconfig
  val vsetfwf = Value("vsetfwf") // vset read old vl write vconfig

  // vec ls
  val vldu = Value("vldu")
  val vstu = Value("vstu")
  val vsegldu = Value("vsegldu")
  val vsegstu = Value("vsegstu")

  val intArithAll = Seq(jmp, brh, i2f, i2v, csr, alu, mul, div, fence, bku)
  // dq0 includes int's iq0 and iq1
  // dq1 includes int's iq2 and iq3
  def dq0OHTypeSeq(implicit p: Parameters): Seq[Seq[Value]] = {
    val intIQParams = p(XSCoreParamsKey).backendParams.intSchdParams.get.issueBlockParams
    val dq0IQNums = intIQParams.size / 2
    val iqParams = intIQParams.take(dq0IQNums)
    val exuParams = iqParams.map(_.exuBlockParams).flatten
    exuParams.map(_.fuConfigs.map(_.fuType))
  }
  def dq1OHTypeSeq(implicit p: Parameters): Seq[Seq[Value]] = {
    val intIQParams = p(XSCoreParamsKey).backendParams.intSchdParams.get.issueBlockParams
    val dq0IQNums = intIQParams.size / 2
    val iqParams = intIQParams.slice(dq0IQNums,intIQParams.size)
    val exuParams = iqParams.map(_.exuBlockParams).flatten
    exuParams.map(_.fuConfigs.map(_.fuType))
  }
  def intDq0All(implicit p: Parameters): Seq[Value] = {
    dq0OHTypeSeq.flatten.distinct
  }
  def intDq0Deq0(implicit p: Parameters): Seq[Value] = {
    val fuTypes = dq0OHTypeSeq(p)(0) ++ dq0OHTypeSeq(p)(2)
    fuTypes.distinct
  }
  def intDq0Deq1(implicit p: Parameters): Seq[Value] = {
    val fuTypes = dq0OHTypeSeq(p)(1) ++ dq0OHTypeSeq(p)(3)
    fuTypes.distinct
  }
  def intDq1All(implicit p: Parameters): Seq[Value] = {
    dq1OHTypeSeq.flatten.distinct
  }
  def intDq1Deq0(implicit p: Parameters): Seq[Value] = {
    val fuTypes = dq1OHTypeSeq(p)(0) ++ dq1OHTypeSeq(p)(2)
    fuTypes.distinct
  }
  def intDq1Deq1(implicit p: Parameters): Seq[Value] = {
    val fuTypes = dq1OHTypeSeq(p)(1) ++ dq1OHTypeSeq(p)(3)
    fuTypes.distinct
  }
  def intBothDeq0(implicit p: Parameters): Seq[Value] = {
    val fuTypes = dq0OHTypeSeq(p)(0).intersect(dq0OHTypeSeq(p)(2)).intersect(dq1OHTypeSeq(p)(0)).intersect(dq1OHTypeSeq(p)(2))
    fuTypes.distinct
  }
  def intBothDeq1(implicit p: Parameters): Seq[Value] = {
    val fuTypes = dq0OHTypeSeq(p)(1).intersect(dq0OHTypeSeq(p)(3)).intersect(dq1OHTypeSeq(p)(1)).intersect(dq1OHTypeSeq(p)(3))
    fuTypes.distinct
  }
  def is0latency(fuType: UInt): Bool = {
    val fuTypes = FuConfig.allConfigs.filter(_.latency == CertainLatency(0)).map(_.fuType)
    FuTypeOrR(fuType, fuTypes)
  }
  val fpArithAll = Seq(f2v)
  val scalaMemAll = Seq(ldu, stu, mou)
  val vecOPI = Seq(vipu, vialuF, vppu, vimac, vidiv)
  val vecOPF = Seq(vfalu, vfma, vfdiv, vfcvt)
  val vecVSET = Seq(vsetiwi, vsetiwf, vsetfwf)
  val vecArith = vecOPI ++ vecOPF
  val vecMem = Seq(vldu, vstu, vsegldu, vsegstu)
  val vecArithOrMem = vecArith ++ vecMem
  val vecAll = vecVSET ++ vecArithOrMem
  val fpOP = fpArithAll ++ Seq(i2f, i2v)
  val scalaNeedFrm = Seq(i2f)
  val vectorNeedFrm = Seq(vfalu, vfma, vfdiv, vfcvt)
  val sharedVf = Seq(vfalu, vfma, vfcvt)

  def num = this.values.size

  def width = log2Up(num)

  def apply() = UInt(width.W)

  def X = BitPat.N(width) // Todo: Don't Care

  def isInt(fuType: UInt): Bool = FuTypeOrR(fuType, intArithAll) || FuTypeOrR(fuType, vsetiwi, vsetiwf)
  def isIntDq0(fuType: UInt)(implicit p: Parameters): Bool = FuTypeOrR(fuType, intDq0All)
  def isIntDq1(fuType: UInt)(implicit p: Parameters): Bool = FuTypeOrR(fuType, intDq1All)
  def isIntDq0Deq0(fuType: UInt)(implicit p: Parameters): Bool = FuTypeOrR(fuType, intDq0Deq0)
  def isIntDq0Deq1(fuType: UInt)(implicit p: Parameters): Bool = FuTypeOrR(fuType, intDq0Deq1)
  def isIntDq1Deq0(fuType: UInt)(implicit p: Parameters): Bool = FuTypeOrR(fuType, intDq1Deq0)
  def isIntDq1Deq1(fuType: UInt)(implicit p: Parameters): Bool = FuTypeOrR(fuType, intDq1Deq1)
  def isBothDeq0(fuType: UInt)(implicit p: Parameters): Bool = FuTypeOrR(fuType, intBothDeq0)
  def isBothDeq1(fuType: UInt)(implicit p: Parameters): Bool = FuTypeOrR(fuType, intBothDeq1)
  def isAlu(fuType: UInt): Bool = FuTypeOrR(fuType, Seq(alu))
  def isBrh(fuType: UInt): Bool = FuTypeOrR(fuType, Seq(brh))

  def isVset(fuType: UInt): Bool = FuTypeOrR(fuType, vecVSET)

  def isJump(fuType: UInt): Bool = FuTypeOrR(fuType, jmp)

  def isFArith(fuType: UInt): Bool = FuTypeOrR(fuType, fpArithAll)

  def isMem(fuType: UInt): Bool = FuTypeOrR(fuType, scalaMemAll)

  def isLoadStore(fuType: UInt): Bool = FuTypeOrR(fuType, ldu, stu)

  def isLoad(fuType: UInt): Bool = FuTypeOrR(fuType, ldu)

  def isStore(fuType: UInt): Bool = FuTypeOrR(fuType, stu)

  def isAMO(fuType: UInt): Bool = FuTypeOrR(fuType, mou)

  def isFence(fuType: UInt): Bool = FuTypeOrR(fuType, fence)

  def isCsr(fuType: UInt): Bool = FuTypeOrR(fuType, csr)

  def isVsetRvfWvf(fuType: UInt): Bool = FuTypeOrR(fuType, vsetfwf)

  def isVArith(fuType: UInt): Bool = FuTypeOrR(fuType, vecArith)

  def isVls(fuType: UInt): Bool = FuTypeOrR(fuType, vldu, vstu, vsegldu, vsegstu)

  def isVnonsegls(fuType: UInt): Bool = FuTypeOrR(fuType, vldu, vstu)

  def isVsegls(futype: UInt): Bool = FuTypeOrR(futype, vsegldu, vsegstu)

  def isVLoad(fuType: UInt): Bool = FuTypeOrR(fuType, vldu, vsegldu)

  def isVStore(fuType: UInt): Bool = FuTypeOrR(fuType, vstu, vsegstu)

  def isVSegLoad(fuType: UInt): Bool = FuTypeOrR(fuType, vsegldu)

  def isVSegStore(fuType: UInt): Bool = FuTypeOrR(fuType, vsegstu)

  def isVNonsegLoad(fuType: UInt): Bool = FuTypeOrR(fuType, vldu)

  def isVNonsegStore(fuType: UInt): Bool = FuTypeOrR(fuType, vstu)

  def isVecOPF(fuType: UInt): Bool = FuTypeOrR(fuType, vecOPF)

  def isVArithMem(fuType: UInt): Bool = FuTypeOrR(fuType, vecArithOrMem) // except vset

  def isDivSqrt(fuType: UInt): Bool = FuTypeOrR(fuType, div)

  def storeIsAMO(fuType: UInt): Bool = FuTypeOrR(fuType, mou)

  def isVppu(fuType: UInt): Bool = FuTypeOrR(fuType, vppu)

  def isScalaNeedFrm(fuType: UInt): Bool = FuTypeOrR(fuType, scalaNeedFrm)

  def isVectorNeedFrm(fuType: UInt): Bool = FuTypeOrR(fuType, vectorNeedFrm)

  object FuTypeOrR {
    def apply(fuType: UInt, fu0: Value, fus: Value*): Bool = {
      apply(fuType, fu0 +: fus)
    }

    def apply(fuType: UInt, fus: Seq[Value]): Bool = {
      fus.map(x => fuType === x.id.U).fold(false.B)(_ || _)
    }

    def apply(fuType: Value, fu0: Value, fus: Value*): Boolean = {
      apply(fuType, fu0 +: fus)
    }

    def apply(fuTupe: Value, fus: Seq[Value]): Boolean = {
      fus.map(x => x == fuTupe).fold(false)(_ || _)
    }
  }

  val functionNameMap = Map(
    jmp -> "jmp",
    brh -> "brh",
    i2f -> "int_to_float",
    i2v -> "int_to_vector",
    f2v -> "float_to_vector",
    csr -> "csr",
    alu -> "alu",
    mul -> "mul",
    div -> "div",
    fence -> "fence",
    bku -> "bku",
    ldu -> "load",
    stu -> "store",
    mou -> "mou",
    vsetiwi -> "vsetiwi",
    vsetiwf -> "vsetiwf",
    vsetfwf -> "vsetfwf",
    vipu -> "vipu",
    vialuF -> "vialuF",
    vldu -> "vldu",
    vstu -> "vstu",
    vppu -> "vppu",
    vimac -> "vimac",
    vidiv -> "vidiv",
    vfalu -> "vfalu",
    vfma -> "vfma",
    vfdiv -> "vfdiv",
    vfcvt -> "vfcvt"
  )
}

