package com.nanni.nes

import com.nanni.nes.cpu.Cpu
import com.nanni.nes.ppu.Ppu

/**
  * Created by fcusumano on 5/9/17.
  */
class Nes {
  val memory = new Ram()
  val stack = new Stack(memory)
  val cpu = new Cpu(memory, stack)
  val ppu = new Ppu()

  reset()

  def reset(): Unit = {
    memory.clear()
    stack.reset()
    cpu.reset()
  }
}
