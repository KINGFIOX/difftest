/***************************************************************************************
* Copyright (c) 2020-2023 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* DiffTest is licensed under Mulan PSL v2.
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

package difftest

import chisel3._

import java.nio.file.{Files, Paths}
import scala.annotation.tailrec
import org.json4s.DefaultFormats

// Main class to generate difftest modules when design is not written in chisel.
class DifftestTop extends Module {
  val difftest_arch_event = DifftestModule(new DiffArchEvent, dontCare = true)
  val difftest_instr_commit = DifftestModule(new DiffInstrCommit, dontCare = true)
  val difftest_trap_event = DifftestModule(new DiffTrapEvent, dontCare = true)
  val difftest_csr_state = DifftestModule(new DiffCSRState, dontCare = true)
  val difftest_hcsr_state = DifftestModule(new DiffHCSRState, dontCare = true)
  val difftest_debug_mode = DifftestModule(new DiffDebugMode, dontCare = true)
  val difftest_trigger_csr_state = DifftestModule(new DiffTriggerCSRState, dontCare = true)
  val difftest_vector_state = DifftestModule(new DiffArchVecRegState, dontCare = true)
  val difftest_vector_csr_state = DifftestModule(new DiffVecCSRState, dontCare = true)
  val difftest_fp_csr_state = DifftestModule(new DiffFpCSRState, dontCare = true)
  val difftest_int_writeback = DifftestModule(new DiffIntWriteback, dontCare = true)
  val difftest_fp_writeback = DifftestModule(new DiffFpWriteback, dontCare = true)
  val difftest_vec_writeback = DifftestModule(new DiffVecWriteback, dontCare = true)
  val difftest_arch_int_reg_state = DifftestModule(new DiffArchIntRegState, dontCare = true)
  val difftest_arch_fp_reg_state = DifftestModule(new DiffArchFpRegState, dontCare = true)
  val difftest_sbuffer_event = DifftestModule(new DiffSbufferEvent, dontCare = true)
  val difftest_store_event = DifftestModule(new DiffStoreEvent, dontCare = true)
  val difftest_load_event = DifftestModule(new DiffLoadEvent, dontCare = true)
  val difftest_atomic_event = DifftestModule(new DiffAtomicEvent, dontCare = true)
  val difftest_itlb_event = DifftestModule(new DiffL1TLBEvent, dontCare = true)
  val difftest_ldtlb_event = DifftestModule(new DiffL1TLBEvent, dontCare = true)
  val difftest_sttlb_event = DifftestModule(new DiffL1TLBEvent, dontCare = true)
  val difftest_l2tlb_event = DifftestModule(new DiffL2TLBEvent, dontCare = true)
  val difftest_irefill_event = DifftestModule(new DiffRefillEvent, dontCare = true)
  val difftest_drefill_event = DifftestModule(new DiffRefillEvent, dontCare = true)
  val difftest_ptwrefill_event = DifftestModule(new DiffRefillEvent, dontCare = true);
  val difftest_lr_sc_event = DifftestModule(new DiffLrScEvent, dontCare = true)
  val difftest_runahead_event = DifftestModule(new DiffRunaheadEvent, dontCare = true)
  val difftest_runahead_commit_event = DifftestModule(new DiffRunaheadCommitEvent, dontCare = true)
  val difftest_runahead_redirect_event = DifftestModule(new DiffRunaheadRedirectEvent, dontCare = true)

  DifftestModule.finish("demo")
}

// Generate simulation interface based on Profile describing the instantiated information of design
class SimTop(profileName: String, numCores: Int) extends Module {
  val profileStr = Files.readString(Paths.get(profileName)) // 从 profile 文件中读取字符串
  // profile 是 json 格式的
  // 他实际上会 filter 这样的数据
  // Map(
  //   "className" -> "MyClass",
  //   "delay" -> 10,
  //   "param1" -> 42,
  //   "param2" -> BigInt(100)
  // )
  val profiles = org.json4s.native.JsonMethods
    .parse(profileStr)
    // 试图从 json 映射到 List[Map[String, Any]] 这种数据类型
    // 帮助 extract 方法在运行时正确地识别 List[Map[String, Any]] 的类型结构
    .extract[List[Map[String, Any]]](DefaultFormats /* 默认的格式化器 */, manifest[List[Map[String, Any]]])
  for (coreid <- 0 until numCores) {
    profiles.filter(_.contains("className")).zipWithIndex.foreach { case (rawProf, idx) =>
      val prof = rawProf.map { case (k, v) =>
        v match {
          case i: BigInt => (k, i.toInt) // convert BigInt to Int
          case x         => (k, x)
        }
      }
      // 这应该是反射了, 获取 className
      val constructor = Class.forName(prof("className").toString).getConstructors()(0)
      val args = constructor.getParameters().toSeq.map { param => prof(param.getName.toString) }
      // args : _ 实际上就是 args : args
      // _* 就是将 args 解包
      // asInstanceOf[DifftestBundle] 就是将其转换为 DifftestBundle 类型
      val inst = constructor.newInstance(args: _*).asInstanceOf[DifftestBundle]
      DifftestModule(gen = inst, dontCare = true, delay = prof("delay").asInstanceOf[Int])
        .suggestName(s"gateway_${coreid}_$idx") // 创建的模块, 名字
    }
  }
  // 找到 profiles 中对应的 cpu
  val dutInfo = profiles.find(_.contains("cpu")).get
  DifftestModule.finish(dutInfo("cpu").asInstanceOf[String])
}

abstract class DifftestApp extends App {

  /**
    * @brief gen params
    *
    * @param profile
    * @param numCores
    */
  case class GenParams(
    profile: Option[String] = None,
    numCores: Int = 1,
  )

  /**
    * @brief 将 args 解析为 GenParams 和 firrtlOpts
    *
    * @param args
    * @return
    */
  def parseArgs(args: Array[String]): (GenParams, Array[String]) = {
    val default = new GenParams()
    var firrtlOpts = Array[String]()

    // 尾递归 ？
    @tailrec
    def nextOption(param: GenParams, list: List[String]): GenParams = {
      list match {
        case Nil => param
        // --profile , 那么就是 GenParams -> profile -> Some(str)
        case "--profile" :: str :: tail     => nextOption(param.copy(profile = Some(str)), tail)
        case "--num-cores" :: value :: tail => nextOption(param.copy(numCores = value.toInt), tail)
        case option :: tail /* 相当于是 match case */ =>
          firrtlOpts :+= option // 其他情况下, 将 option 添加到 firrtlOpts 中
          nextOption(param, tail)
      }
    }

    (nextOption(default, args.toList), firrtlOpts)
  }

  // 设置 gateway, newArgs == args
  val newArgs = DifftestModule.parseArgs(args)

  val (param, firrtlOpts) = parseArgs(newArgs)

  val gen = if (param.profile.isDefined) { () =>
    new SimTop(param.profile.get, param.numCores)
  } else { () =>
    new DifftestTop
  }
}
