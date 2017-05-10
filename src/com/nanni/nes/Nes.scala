package com.nanni.nes

import com.nanni.nes.cpu.Cpu

/**
  * Created by fcusumano on 5/9/17.
  */
class Nes {
  val memory = new Ram()
  val stack = new Stack(memory)
  val cpu = new Cpu(memory, stack)

  reset()

  def reset(): Unit = {
    memory.clear()
    stack.reset()
    cpu.reset()
  }
}
