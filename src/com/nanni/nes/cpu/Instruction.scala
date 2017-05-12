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

  val operandSize: Int = mode match {
    case AddressingMode.Implied |
         AddressingMode.Accumulator => 0
    case AddressingMode.Immediate |
      AddressingMode.ZeroPage |
      AddressingMode.ZeroPageX |
      AddressingMode.ZeroPageY |
      AddressingMode.Relative => 1
    case _ => 2
  }

  val size: Int = operandSize + 1
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