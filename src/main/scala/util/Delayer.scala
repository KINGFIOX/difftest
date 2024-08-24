/***************************************************************************************
 * Copyright (c) 2024 Beijing Institute of Open Source Chip (BOSC)
 * Copyright (c) 2020-2024 Institute of Computing Technology, Chinese Academy of Sciences
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

package difftest.util

import chisel3._
import chisel3.util._

private class Delayer[T <: Data](gen: T, n_cycles: Int) extends Module {
  val i = IO(Input(chiselTypeOf(gen)))
  val o = IO(Output(chiselTypeOf(gen)))
}

private class DelayReg[T <: Data](gen: T, n_cycles: Int) extends Delayer(gen, n_cycles) {
  var r = WireInit(i)
  // 包几层 RegNext
  for (_ <- 0 until n_cycles) {
    r = RegNext(r, 0.U.asTypeOf(gen))
  }
  o := r
}

private class DelayMem[T <: Data](gen: T, n_cycles: Int) extends Delayer(gen, n_cycles) {
  val mem = Mem(n_cycles, chiselTypeOf(gen))
  val ptr = RegInit(0.U(log2Ceil(n_cycles).W)) // 用于指示当前写入的位置
  val init_flag = RegInit(false.B) // 用于指示是否已经初始化
  mem(ptr) := i // io input
  ptr := ptr + 1.U
  when(ptr === (n_cycles - 1).U) {
    init_flag := true.B
    ptr := 0.U
  }
  // 相当于是: 每周期移位
  o := Mux(init_flag, mem(ptr), 0.U.asTypeOf(gen))
}

object Delayer {
  def apply[T <: Data](gen: T, n_cycles: Int, useMem: Boolean = false): T = {
    if (n_cycles > 0) {
      val delayer = if (useMem) {
        Module(new DelayMem(gen, n_cycles))
      } else {
        Module(new DelayReg(gen, n_cycles))
      }
      delayer.i := gen
      delayer.o
    } else { // 否则就是: 没有延迟
      gen
    }
  }
}
