package com.nanni.nes.cpu

import com.nanni.nes.cpu.AddressingMode.AddressingMode

/**
  * Created by fcusumano on 5/8/17.
  */
class Instruction(val opcode: Int,
                  val name: String,
                  private val function: (AddressingMode) =>Unit,
                  val mode: AddressingMode,
                  val cycles: Int) {
  def run(): Int = {
    if(function != null)
      function(mode)
    cycles
  }
}

object Instruction {
  def apply(opcode: Int,
            name: String,
            function: (AddressingMode) => Unit,
            mode: AddressingMode,
            cycles: Int): Instruction = {
    new Instruction(opcode, name, function, mode, cycles)
  }
}