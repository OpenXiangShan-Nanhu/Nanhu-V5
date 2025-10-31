/***************************************************************************************
* Copyright (c) 2024 Beijing Institute of Open Source Chip (BOSC)
* Copyright (c) 2020-2024 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

import mill._
import scalalib._
import $file.dependencies.`rocket-chip`.common

def defaultVersions = Map(
  "scala"         -> "2.13.16",
  "scalatest"     -> "3.2.7",
  "chisel"        -> "6.7.0",
  "chisel-plugin" -> "6.7.0",
  "chiseltest"    -> "6.0.0",
  "llvm-firtool"  -> "1.62.1"
)

def getVersion(dep: String, org: String = "org.chipsalliance", cross: Boolean = false) = {
  val version = sys.env.getOrElse(dep + "Version", defaultVersions(dep))
  if (cross) {
    ivy"$org:::$dep:$version"
  } else {
    ivy"$org::$dep:$version"
  }
}

trait CommonModule extends ScalaModule {
  override def scalaVersion = defaultVersions("scala")
  override def scalacPluginIvyDeps = Agg(getVersion("chisel-plugin", cross = true))
  override def scalacOptions = super.scalacOptions() ++ Agg("-Ymacro-annotations", "-Ytasty-reader")
  override def ivyDeps = super.ivyDeps() ++ Agg(
    getVersion("chisel"),
    getVersion("chiseltest", "edu.berkeley.cs"),
    ivy"org.chipsalliance:llvm-firtool:1.62.1"
  )
}

object cde extends CommonModule {
  override def millSourcePath = os.pwd / "dependencies" / "cde" / "cde"
}

object diplomacy extends CommonModule {
  override def millSourcePath = os.pwd / "dependencies" / "diplomacy" / "diplomacy"
  override def moduleDeps = super.moduleDeps ++ Seq(cde)
  def sourcecodeIvy = ivy"com.lihaoyi::sourcecode:0.3.1"
}

object rocketchip extends millbuild.dependencies.`rocket-chip`.common.RocketChipModule {
  override def millSourcePath = os.pwd / "dependencies" / "rocket-chip"
  def scalaVersion: T[String] = T(defaultVersions("scala"))
  def chiselModule = None
  def chiselPluginJar = None
  def chiselIvy = Some(getVersion("chisel"))
  def chiselPluginIvy = Some(getVersion("chisel-plugin", cross = true))
  def macrosModule = macros
  def hardfloatModule = hardfloat
  def cdeModule = cde
  def diplomacyModule = diplomacy
  def diplomacyIvy = None
  def mainargsIvy = ivy"com.lihaoyi::mainargs:0.5.0"
  def json4sJacksonIvy = ivy"org.json4s::json4s-jackson:4.0.5"

  object hardfloat extends CommonModule {
    override def millSourcePath = os.pwd / "dependencies" / "hardfloat" / "hardfloat"
  }

  object macros extends millbuild.dependencies.`rocket-chip`.common.MacrosModule with SbtModule {
    def scalaVersion: T[String] = T(defaultVersions("scala"))
    def scalaReflectIvy = ivy"org.scala-lang:scala-reflect:${defaultVersions("scala")}"
  }
}

object xsutils extends SbtModule with CommonModule {
  override def millSourcePath = os.pwd / "dependencies" / "xs-utils"
  override def moduleDeps = super.moduleDeps ++ Seq(rocketchip, cde)
}

object yunsuan extends SbtModule with CommonModule {
  override def millSourcePath = os.pwd / "dependencies" / "YunSuan"
}

object difftest extends SbtModule with CommonModule {
  override def millSourcePath = os.pwd / "dependencies" / "difftest"
}

object xiangshan extends SbtModule with CommonModule {

  override def millSourcePath = os.pwd
  override def moduleDeps = super.moduleDeps ++ Seq(rocketchip, difftest, yunsuan, xsutils)
  override def scalacOptions = super.scalacOptions() ++ Agg("-deprecation", "-feature")
  def mainClass = Some("top.SocGenerator")

  object test extends SbtModuleTests with TestModule.ScalaTest {
    override def ivyDeps = super.ivyDeps() ++ Agg(getVersion("scalatest", "org.scalatest"))
  }
}
