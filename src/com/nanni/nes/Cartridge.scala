package com.nanni.nes

import com.nanni.nes.TvSystem.TvSystem
import com.nanni.nes.cpu.{AddressingMode, Instructions}

/**
  * Created by Nanni on 08/05/2017.
  */
class Cartridge(bytes: Array[Byte]) {
  if(!bytes.take(4).sameElements(Array('N', 'E', 'S', 0x1A))) {
    throw new Exception("Invalid iNes cartridge!")
  }

  val prgPages: Int = bytes(5)
  val chrPages: Int= bytes(6)
  val mapperNumber: Int = (bytes(7) >> 4) & 0xF
  val hasPrgRam: Boolean = (bytes(10 >> 4) & 0x1) != 0
  val tvSystem: TvSystem = TvSystem.fromValue(bytes(10) & 0x03)

  val trainer: Array[Byte] = new Array[Byte](0)
  val prgRom: Array[Byte] =  bytes.slice(0x0F + trainer.size, 0x0F + trainer.size + 0x4000 * prgPages)
  val chrRom: Array[Byte] = bytes.slice(0x0F + trainer.size + prgRom.size, 0x0F + trainer.size + prgRom.size + 0x2000 * chrPages)

  def disassembly(instructions: Instructions): String = {
    val builder = new StringBuilder
    var index = 0

    while(index < prgRom.length) {
      val opcode = prgRom(index)
      val instruction = instructions.getInstruction(opcode)

      if(instruction.isDefined) {

        builder.append(instruction.get.name + " ")

        instruction.get.mode match {
          case AddressingMode.Accumulator => builder.append("A")
          case AddressingMode.Immediate =>
            builder.append(s"#${prgRom(index)}")
            index += 1
          case AddressingMode.ZeroPage =>
            builder.append("$" + s"%02X".format(prgRom(index)))
            index += 1
          case AddressingMode.ZeroPageX =>
            builder.append("$" + s"%02X,X".format(prgRom(index)))
            index += 1
          case AddressingMode.ZeroPageY =>
            builder.append("$" + s"%02X,Y".format(prgRom(index)))
            index += 1
          case AddressingMode.Relative =>
            builder.append(prgRom(index).toInt)
            builder += 1
          case _ => ()
        }

      } else {
        builder.append(s"; Unknown instruction (opcode: ${"$%02X".format(opcode)})")
      }

      builder.append("\n")

      index += 1
    }

    builder.toString()
  }
}
