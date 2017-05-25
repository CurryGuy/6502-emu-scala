package com.nanni.nes

import com.nanni.nes.cpu.Cpu
import com.nanni.nes.ppu.Ppu

/**
  * Created by fcusumano on 5/9/17.
  */
class Nes {
  val ppu = new Ppu()
  val memory = new Memory(null, ppu)
  val stack = new Stack(memory)
  val cpu = new Cpu(memory, stack)

  init()

  def init(): Unit = {
    memory.init()
    stack.init()
    cpu.init()
  }

  def reset(): Unit = {
    memory.clear()
    stack.reset()
    cpu.reset()
  }
}