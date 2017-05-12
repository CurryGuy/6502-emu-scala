package com.nanni.nes.cpu

import com.nanni.nes.cpu.AddressingMode.AddressingMode

/**
  * Created by fcusumano on 5/8/17.
  */
class Instructions(private val cpu: Cpu) {
  def getInstruction(opcode: Int): Option[Instruction] = {
    table.find(_.opcode == opcode)
  }

  def getInstruction(name: String, mode: AddressingMode): Option[Instruction] = {
    table.find(i => i.name == name && i.mode == mode)
  }

  def getOpcode(name: String, mode: AddressingMode): Option[Int] = getInstruction(name, mode) match {
    case Some(i) => Some(i.opcode)
    case None => None
  }

  private def checkOverflow(a: Int, b: Int, result: Int): Boolean = {
    checkNegative(~(a ^ b) & (a ^ result))
  }

  private def checkNegative(value: Int): Boolean = (value & 0x80) != 0

  private def branch(mode: AddressingMode, condition: Boolean): Unit = {
    val op = cpu.fetchOperand(mode)
    if(condition) {
      if(((cpu.PC + op.address) & 0xFF00) != (cpu.PC & 0xFF00)) {
        cpu.incrementCycles(1)
      }

      cpu.PC += op.address
      cpu.incrementCycles(1)
    }
  }

  private val table = Array[Instruction](
    /* ADC */
    Instruction(0x69, "ADC", ADC, AddressingMode.Immediate, 2),
    Instruction(0x65, "ADC", ADC, AddressingMode.ZeroPage, 3),
    Instruction(0x75, "ADC", ADC, AddressingMode.ZeroPageX, 4),
    Instruction(0x6D, "ADC", ADC, AddressingMode.Absolute, 4),
    Instruction(0x7D, "ADC", ADC, AddressingMode.AbsoluteX, 4),
    Instruction(0x79, "ADC", ADC, AddressingMode.AbsoluteY, 4),
    Instruction(0x61, "ADC", ADC, AddressingMode.IndexedIndirect, 6),
    Instruction(0x71, "ADC", ADC, AddressingMode.IndirectIndexed, 5),

    /* AND */
    Instruction(0x29, "AND", AND, AddressingMode.Immediate, 2),
    Instruction(0x25, "AND", AND, AddressingMode.ZeroPage, 3),
    Instruction(0x35, "AND", AND, AddressingMode.ZeroPageX, 4),
    Instruction(0x2D, "AND", AND, AddressingMode.Absolute, 4),
    Instruction(0x3D, "AND", AND, AddressingMode.AbsoluteX, 4),
    Instruction(0x39, "AND", AND, AddressingMode.AbsoluteY, 4),
    Instruction(0x21, "AND", AND, AddressingMode.IndexedIndirect, 6),
    Instruction(0x31, "AND", AND, AddressingMode.IndirectIndexed, 5),

    /* ASL */
    Instruction(0x0A, "ASL", ASL, AddressingMode.Accumulator, 2),
    Instruction(0x06, "ASL", ASL, AddressingMode.ZeroPage, 5),
    Instruction(0x16, "ASL", ASL, AddressingMode.ZeroPageX, 6),
    Instruction(0x0E, "ASL", ASL, AddressingMode.Absolute, 6),
    Instruction(0x1E, "ASL", ASL, AddressingMode.AbsoluteX, 7),

    /* BCC */
    Instruction(0x90, "BCC", BCC, AddressingMode.Relative, 2),

    /* BCS */
    Instruction(0xB0, "BCS", BCS, AddressingMode.Relative, 2),

    /* BEQ */
    Instruction(0xF0, "BEQ", BEQ, AddressingMode.Relative, 2),

    /* BIT */
    Instruction(0x24, "BIT", BIT, AddressingMode.ZeroPage, 3),
    Instruction(0x2C, "BIT", BIT, AddressingMode.AbsoluteX, 4),

    /* BMI */
    Instruction(0x30, "BMI", BMI, AddressingMode.Relative, 2),

    /* BNE */
    Instruction(0xD0, "BNE", BNE, AddressingMode.Relative, 2),

    /* BPL */
    Instruction(0x10, "BPL", BPL, AddressingMode.Relative, 2),

    /* BRK */
    Instruction(0x00, "BRK", BRK, AddressingMode.Implied, 7),

    /* BVC */
    Instruction(0x50, "BVC", BVC, AddressingMode.Relative, 2),

    /* BVS */
    Instruction(0x70, "BVS", BVS, AddressingMode.Relative, 2),

    /* CLC */
    Instruction(0x18, "CLC", CLC, AddressingMode.Implied, 2),

    /* CLD */
    Instruction(0xD8, "CLD", CLD, AddressingMode.Implied, 2),

    /* CLI */
    Instruction(0x58, "CLI", CLI, AddressingMode.Implied, 2),

    /* CLV */
    Instruction(0xB8, "CLV", CLV, AddressingMode.Implied, 2),

    /* CMP */
    Instruction(0xC9, "CMP", CMP, AddressingMode.Immediate, 2),
    Instruction(0xC5, "CMP", CMP, AddressingMode.ZeroPage, 3),
    Instruction(0xD5, "CMP", CMP, AddressingMode.ZeroPageX, 4),
    Instruction(0xCD, "CMP", CMP, AddressingMode.Absolute, 4),
    Instruction(0xDD, "CMP", CMP, AddressingMode.AbsoluteX, 4),
    Instruction(0xD9, "CMP", CMP, AddressingMode.AbsoluteY, 4),
    Instruction(0xC1, "CMP", CMP, AddressingMode.IndexedIndirect, 6),
    Instruction(0xD1, "CMP", CMP, AddressingMode.IndirectIndexed, 5),

    /* CPX */
    Instruction(0xE0, "CPX", CPX, AddressingMode.Immediate, 2),
    Instruction(0xE4, "CPX", CPX, AddressingMode.ZeroPage, 3),
    Instruction(0xEC, "CPX", CPX, AddressingMode.Absolute, 4),

    /* CPY */
    Instruction(0xC0, "CPY", CPY, AddressingMode.Immediate, 2),
    Instruction(0xC4, "CPY", CPY, AddressingMode.ZeroPage, 3),
    Instruction(0xCC, "CPY", CPY, AddressingMode.Absolute, 4),

    /* DEC */
    Instruction(0xC6, "DEC", DEC, AddressingMode.ZeroPage, 5),
    Instruction(0xD6, "DEC", DEC, AddressingMode.ZeroPageX, 6),
    Instruction(0xCE, "DEC", DEC, AddressingMode.Absolute, 6),
    Instruction(0xDE, "DEC", DEC, AddressingMode.AbsoluteX, 7),

    /* DEX */
    Instruction(0xCA, "DEX", DEX, AddressingMode.Implied, 2),

    /* DEY */
    Instruction(0x88, "DEY", DEY, AddressingMode.Implied, 2),

    /* EOR */
    Instruction(0x49, "EOR", EOR, AddressingMode.Immediate, 2),
    Instruction(0x45, "EOR", EOR, AddressingMode.ZeroPage, 3),
    Instruction(0x55, "EOR", EOR, AddressingMode.ZeroPageX, 4),
    Instruction(0x4D, "EOR", EOR, AddressingMode.Absolute, 4),
    Instruction(0x5D, "EOR", EOR, AddressingMode.AbsoluteX, 4),
    Instruction(0x59, "EOR", EOR, AddressingMode.AbsoluteY, 4),
    Instruction(0x41, "EOR", EOR, AddressingMode.IndexedIndirect, 6),
    Instruction(0x51, "EOR", EOR, AddressingMode.IndirectIndexed, 5),

    /* INC */
    Instruction(0xE6, "INC", INC, AddressingMode.ZeroPage, 5),
    Instruction(0xF6, "INC", INC, AddressingMode.ZeroPageX, 6),
    Instruction(0xEE, "INC", INC, AddressingMode.Absolute, 6),
    Instruction(0xFE, "INC", INC, AddressingMode.AbsoluteX, 7),

    /* INX */
    Instruction(0xE8, "INX", INX, AddressingMode.Implied, 2),

    /* INY */
    Instruction(0xC8, "INY", INY, AddressingMode.Implied, 2),

    /* JMP */
    Instruction(0x4C, "JMP", JMP, AddressingMode.Absolute, 3),
    Instruction(0x6C, "JMP", JMP, AddressingMode.Indirect, 5),

    /* JSR */
    Instruction(0x20, "JSR", JSR, AddressingMode.Absolute, 6),

    /* LDA */
    Instruction(0xA9, "LDA", LDA, AddressingMode.Immediate, 2),
    Instruction(0xA5, "LDA", LDA, AddressingMode.ZeroPage, 3),
    Instruction(0xB5, "LDA", LDA, AddressingMode.ZeroPageX, 4),
    Instruction(0xAD, "LDA", LDA, AddressingMode.Absolute, 4),
    Instruction(0xBD, "LDA", LDA, AddressingMode.AbsoluteX, 4),
    Instruction(0xB9, "LDA", LDA, AddressingMode.AbsoluteY, 4),
    Instruction(0xA1, "LDA", LDA, AddressingMode.IndexedIndirect, 6),
    Instruction(0xB1, "LDA", LDA, AddressingMode.IndirectIndexed, 5),

    /* LDX */
    Instruction(0xA2, "LDX", LDX, AddressingMode.Immediate, 2),
    Instruction(0xA6, "LDX", LDX, AddressingMode.ZeroPage, 3),
    Instruction(0xB6, "LDX", LDX, AddressingMode.ZeroPageY, 4),
    Instruction(0xAE, "LDX", LDX, AddressingMode.Absolute, 4),
    Instruction(0xBE, "LDX", LDX, AddressingMode.AbsoluteY, 4),

    /* LDY */
    Instruction(0xA0, "LDY", LDY, AddressingMode.Immediate, 2),
    Instruction(0xA4, "LDY", LDY, AddressingMode.ZeroPage, 3),
    Instruction(0xB4, "LDY", LDY, AddressingMode.ZeroPageX, 4),
    Instruction(0xAC, "LDY", LDY, AddressingMode.Absolute, 4),
    Instruction(0xBC, "LDY", LDY, AddressingMode.AbsoluteX, 4),

    /* LSR */
    Instruction(0x4A, "LSR", LSR, AddressingMode.Accumulator, 2),
    Instruction(0x46, "LSR", LSR, AddressingMode.ZeroPage, 5),
    Instruction(0x56, "LSR", LSR, AddressingMode.ZeroPageX, 6),
    Instruction(0x4E, "LSR", LSR, AddressingMode.Absolute, 6),
    Instruction(0x5E, "LSR", LSR, AddressingMode.AbsoluteX, 7),

    /* NOP */
    Instruction(0xEA, "NOP", NOP, AddressingMode.Implied, 2),

    /* ORA */
    Instruction(0x09, "ORA", ORA, AddressingMode.Immediate, 2),
    Instruction(0x05, "ORA", ORA, AddressingMode.ZeroPage, 3),
    Instruction(0x15, "ORA", ORA, AddressingMode.ZeroPageX, 4),
    Instruction(0x0D, "ORA", ORA, AddressingMode.Absolute, 4),
    Instruction(0x1D, "ORA", ORA, AddressingMode.AbsoluteX, 4),
    Instruction(0x19, "ORA", ORA, AddressingMode.AbsoluteY, 4),
    Instruction(0x01, "ORA", ORA, AddressingMode.IndexedIndirect, 6),
    Instruction(0x11, "ORA", ORA, AddressingMode.IndirectIndexed, 5),

    /* PHA */
    Instruction(0x48, "PHA", PHA, AddressingMode.Implied, 3),

    /* PHP */
    Instruction(0x08, "PHP", PHP, AddressingMode.Implied, 3),

    /* PLA */
    Instruction(0x68, "PLA", PLA, AddressingMode.Implied, 4),

    /* PLP */
    Instruction(0x28, "PLP", PLP, AddressingMode.Implied, 4),

    /* ROR */
    Instruction(0x6A, "ROR", ROR, AddressingMode.Accumulator, 2),
    Instruction(0x66, "ROR", ROR, AddressingMode.ZeroPage, 5),
    Instruction(0x76, "ROR", ROR, AddressingMode.ZeroPageX, 6),
    Instruction(0x6E, "ROR", ROR, AddressingMode.Absolute, 6),
    Instruction(0x7E, "ROR", ROR, AddressingMode.AbsoluteX, 7),

    /* RTI */
    Instruction(0x40, "RTI", RTI, AddressingMode.Implied, 6),

    /* RTS */
    Instruction(0x60, "RTS", RTS, AddressingMode.Implied, 6),

    /* SBC */
    Instruction(0xE9, "SBC", SBC, AddressingMode.Immediate, 2),
    Instruction(0xE5, "SBC", SBC, AddressingMode.ZeroPage, 3),
    Instruction(0xF5, "SBC", SBC, AddressingMode.ZeroPageX, 4),
    Instruction(0xED, "SBC", SBC, AddressingMode.Absolute, 4),
    Instruction(0xFD, "SBC", SBC, AddressingMode.AbsoluteX, 4),
    Instruction(0xF9, "SBC", SBC, AddressingMode.AbsoluteY, 4),
    Instruction(0xE1, "SBC", SBC, AddressingMode.IndexedIndirect, 6),
    Instruction(0xF1, "SBC", SBC, AddressingMode.IndirectIndexed, 5),

    /* SEC */
    Instruction(0x38, "SEC", SEC, AddressingMode.Implied, 2),

    /* SED */
    Instruction(0xF8, "SED", SED, AddressingMode.Implied, 2),

    /* SEI */
    Instruction(0x78, "SEI", SEI, AddressingMode.Implied, 2),

    /* STA */
    Instruction(0x85, "STA", STA, AddressingMode.ZeroPage, 3),
    Instruction(0x95, "STA", STA, AddressingMode.ZeroPageX, 4),
    Instruction(0x8D, "STA", STA, AddressingMode.Absolute, 4),
    Instruction(0x9D, "STA", STA, AddressingMode.AbsoluteX, 5),
    Instruction(0x99, "STA", STA, AddressingMode.AbsoluteY, 5),
    Instruction(0x81, "STA", STA, AddressingMode.IndexedIndirect, 6),
    Instruction(0x91, "STA", STA, AddressingMode.IndirectIndexed, 6),

    /* STX */
    Instruction(0x86, "STX", STX, AddressingMode.ZeroPage, 3),
    Instruction(0x96, "STX", STX, AddressingMode.ZeroPageY, 4),
    Instruction(0x8E, "STX", STX, AddressingMode.Absolute, 4),

    /* STY */
    Instruction(0x84, "STY", STY, AddressingMode.ZeroPage, 3),
    Instruction(0x94, "STY", STY, AddressingMode.ZeroPageX, 4),
    Instruction(0x8C, "STY", STY, AddressingMode.Absolute, 4),

    /* TAX */
    Instruction(0xAA, "TAX", TAX, AddressingMode.Implied, 2),

    /* TAY */
    Instruction(0xA8, "TAY", TAY, AddressingMode.Implied, 2),

    /* TSX */
    Instruction(0xBA, "TSX", TSX, AddressingMode.Implied, 2),

    /* TXA */
    Instruction(0x8A, "TXA", TXA, AddressingMode.Implied, 2),

    /* TXS */
    Instruction(0x9A, "TXS", TXS, AddressingMode.Implied, 2),

    /* TYA */
    Instruction(0x98, "TYA", TYA, AddressingMode.Implied, 2)
  )

  def ADC(mode: AddressingMode): Unit = {
    val op = cpu.fetchOperand(mode)
    val data = cpu.readOperand(op)
    val a: Int = cpu.A

    val result: Int = a + data + (if(cpu.getFlag(CpuFlag.Carry)) 1 else 0)

    cpu.A := result

    cpu.setFlag(CpuFlag.Carry, result > 0xFF)
    cpu.setFlag(CpuFlag.Zero, cpu.A == 0)
    cpu.setFlag(CpuFlag.Overflow, checkOverflow(a, data, result))
    cpu.setFlag(CpuFlag.Negative, checkNegative(result))
  }

  def AND(mode: AddressingMode): Unit = {
    val op = cpu.fetchOperand(mode)
    val result: Int = cpu.A & cpu.readOperand(op)

    cpu.A := result

    cpu.setFlag(CpuFlag.Zero, cpu.A == 0)
    cpu.setFlag(CpuFlag.Negative, checkNegative(result))
  }

  def ASL(mode: AddressingMode): Unit = {
    val op = cpu.fetchOperand(mode)
    val data = cpu.readOperand(op)
    val result: Int = data << 1

    cpu.writeOperand(op, result)

    cpu.setFlag(CpuFlag.Carry, (data & 0x80) != 0)
    cpu.setFlag(CpuFlag.Zero, cpu.A == 0)
    cpu.setFlag(CpuFlag.Negative, checkNegative(result))
  }

  def BCC(mode: AddressingMode): Unit = {
    branch(mode, !cpu.getFlag(CpuFlag.Carry))
  }

  def BCS(mode: AddressingMode): Unit = {
    branch(mode, cpu.getFlag(CpuFlag.Carry))
  }

  def BEQ(mode: AddressingMode): Unit = {
    branch(mode, cpu.getFlag(CpuFlag.Zero))
  }

  def BIT(mode: AddressingMode): Unit = {
    val op = cpu.fetchOperand(mode)
    val data = cpu.readOperand(op)

    val result = cpu.A & data

    cpu.setFlag(CpuFlag.Zero, result == 0)
    cpu.setFlag(CpuFlag.Overflow, ((1 << 6) & data) != 0)
    cpu.setFlag(CpuFlag.Negative, ((1 << 7) & data) != 0)
  }

  def BMI(mode: AddressingMode): Unit = {
    branch(mode, cpu.getFlag(CpuFlag.Negative))
  }

  def BNE(mode: AddressingMode): Unit = {
    branch(mode, !cpu.getFlag(CpuFlag.Zero))
  }

  def BPL(mode: AddressingMode): Unit = {
    branch(mode, !cpu.getFlag(CpuFlag.Negative))
  }

  def BRK(mode: AddressingMode): Unit = {
    cpu.stack.pushWord(cpu.PC)
    cpu.stack.pushByte(cpu.P)

    cpu.PC := cpu.mem.readWord(0xFFFE)

    cpu.setFlag(CpuFlag.Break, set = true)
  }

  def BVC(mode: AddressingMode): Unit = {
    branch(mode, !cpu.getFlag(CpuFlag.Overflow))
  }

  def BVS(mode: AddressingMode): Unit = {
    branch(mode, cpu.getFlag(CpuFlag.Overflow))
  }

  def CLC(mode: AddressingMode): Unit = {
    cpu.setFlag(CpuFlag.Carry, set = false)
  }

  def CLD(mode: AddressingMode): Unit = {
    cpu.setFlag(CpuFlag.Decimal, set = false)
  }

  def CLI(mode: AddressingMode): Unit = {
    cpu.setFlag(CpuFlag.Interrupt, set = false)
  }

  def CLV(mode: AddressingMode): Unit = {
    cpu.setFlag(CpuFlag.Overflow, set = false)
  }

  def CMP(mode: AddressingMode): Unit = {
    val op = cpu.fetchOperand(mode)
    val data = cpu.readOperand(op)

    cpu.setFlag(CpuFlag.Carry, cpu.A >= data)
    cpu.setFlag(CpuFlag.Zero, cpu.A == data)
    cpu.setFlag(CpuFlag.Negative, cpu.A < data)
  }

  def CPX(mode: AddressingMode): Unit = {
    val op = cpu.fetchOperand(mode)
    val data = cpu.readOperand(op)

    cpu.setFlag(CpuFlag.Carry, cpu.X >= data)
    cpu.setFlag(CpuFlag.Zero, cpu.X == data)
    cpu.setFlag(CpuFlag.Negative, cpu.X < data)
  }

  def CPY(mode: AddressingMode): Unit = {
    val op = cpu.fetchOperand(mode)
    val data = cpu.readOperand(op)

    cpu.setFlag(CpuFlag.Carry, cpu.Y >= data)
    cpu.setFlag(CpuFlag.Zero, cpu.Y == data)
    cpu.setFlag(CpuFlag.Negative, cpu.Y < data)
  }

  def DEC(mode: AddressingMode): Unit = {
    val op = cpu.fetchOperand(mode)
    val data = cpu.readOperand(op)
    val result = data - 1

    cpu.writeOperand(op, result)

    cpu.setFlag(CpuFlag.Zero, result == 0)
    cpu.setFlag(CpuFlag.Negative, checkNegative(result))
  }

  def DEX(mode: AddressingMode): Unit = {
    val result = cpu.X - 1

    cpu.X := result

    cpu.setFlag(CpuFlag.Zero, result == 0)
    cpu.setFlag(CpuFlag.Negative, checkNegative(result))
  }

  def DEY(mode: AddressingMode): Unit = {
    val result = cpu.Y - 1

    cpu.Y := result

    cpu.setFlag(CpuFlag.Zero, result == 0)
    cpu.setFlag(CpuFlag.Negative, checkNegative(result))
  }

  def EOR(mode: AddressingMode): Unit = {
    val op = cpu.fetchOperand(mode)
    val data = cpu.readOperand(op)
    val result = cpu.A ^ data

    cpu.A := result

    cpu.setFlag(CpuFlag.Zero, result == 0)
    cpu.setFlag(CpuFlag.Negative, checkNegative(result))
  }

  def INC(mode: AddressingMode): Unit = {
    val op = cpu.fetchOperand(mode)
    val data = cpu.readOperand(op)
    val result = data + 1

    cpu.writeOperand(op, result)

    cpu.setFlag(CpuFlag.Zero, result == 0)
    cpu.setFlag(CpuFlag.Negative, checkNegative(result))
  }

  def INX(mode: AddressingMode): Unit = {
    val result = cpu.X + 1

    cpu.X := result

    cpu.setFlag(CpuFlag.Zero, result == 0)
    cpu.setFlag(CpuFlag.Negative, checkNegative(result))
  }

  def INY(mode: AddressingMode): Unit = {
    val result = cpu.Y + 1

    cpu.Y := result

    cpu.setFlag(CpuFlag.Zero, result == 0)
    cpu.setFlag(CpuFlag.Negative, checkNegative(result))
  }

  def JMP(mode: AddressingMode): Unit = {
    val op = cpu.fetchOperand(mode)

    cpu.PC := op.address
  }

  def JSR(mode: AddressingMode): Unit = {
    val op = cpu.fetchOperand(mode)

    cpu.stack.pushWord(cpu.PC)
    cpu.PC := op.address
  }

  def LDA(mode: AddressingMode): Unit = {
    val op = cpu.fetchOperand(mode)
    val data = cpu.readOperand(op)

    cpu.A := data

    cpu.setFlag(CpuFlag.Zero, data == 0)
    cpu.setFlag(CpuFlag.Negative, checkNegative(data))
  }

  def LDX(mode: AddressingMode): Unit = {
    val op = cpu.fetchOperand(mode)
    val data = cpu.readOperand(op)

    cpu.X := data

    cpu.setFlag(CpuFlag.Zero, data == 0)
    cpu.setFlag(CpuFlag.Negative, checkNegative(data))
  }

  def LDY(mode: AddressingMode): Unit = {
    val op = cpu.fetchOperand(mode)
    val data = cpu.readOperand(op)

    cpu.Y := data

    cpu.setFlag(CpuFlag.Zero, data == 0)
    cpu.setFlag(CpuFlag.Negative, checkNegative(data))
  }

  def LSR(mode: AddressingMode): Unit = {
    val op = cpu.fetchOperand(mode)
    val data = cpu.readOperand(op)

    val result = data >>> 1

    cpu.writeOperand(op, result)

    cpu.setFlag(CpuFlag.Carry, (data & 0x1) != 0)
    cpu.setFlag(CpuFlag.Zero, result == 0)
    cpu.setFlag(CpuFlag.Negative, checkNegative(result))
  }

  def NOP(mode: AddressingMode): Unit = {
  }

  def ORA(mode: AddressingMode): Unit = {
    val op = cpu.fetchOperand(mode)
    val data = cpu.readOperand(op)

    val result = data | cpu.A

    cpu.A := result

    cpu.setFlag(CpuFlag.Zero, result == 0)
    cpu.setFlag(CpuFlag.Negative, checkNegative(result))
  }

  def PHA(mode: AddressingMode): Unit = {
    cpu.stack.pushByte(cpu.A)
  }

  def PHP(mode: AddressingMode): Unit = {
    cpu.stack.pushByte(cpu.P)
  }

  def PLA(mode: AddressingMode): Unit = {
    val a = cpu.stack.popByte()

    cpu.A := a

    cpu.setFlag(CpuFlag.Zero, a == 0)
    cpu.setFlag(CpuFlag.Negative, checkNegative(a))
  }

  def PLP(mode: AddressingMode): Unit = {
    val p = cpu.stack.popByte()
    cpu.P := p
  }

  def ROL(mode: AddressingMode): Unit = {
    val op = cpu.fetchOperand(mode)
    val data = cpu.readOperand(op)

    var result = data << 1

    if(cpu.getFlag(CpuFlag.Carry))
      result |= 0x01
    else
      result &= ~0x01

    cpu.writeOperand(op, result)

    cpu.setFlag(CpuFlag.Carry, checkNegative(data))
    cpu.setFlag(CpuFlag.Zero, cpu.A == 0)
    cpu.setFlag(CpuFlag.Negative, checkNegative(result))
  }

  def ROR(mode: AddressingMode): Unit = {
    val op = cpu.fetchOperand(mode)
    val data = cpu.readOperand(op)

    var result = data >> 1

    if(cpu.getFlag(CpuFlag.Carry))
      result |= 0x80
    else
      result &= ~0x80

    cpu.writeOperand(op, result)

    cpu.setFlag(CpuFlag.Carry, (data & 0x01) != 0)
    cpu.setFlag(CpuFlag.Zero, cpu.A == 0)
    cpu.setFlag(CpuFlag.Negative, checkNegative(result))
  }

  def RTI(mode: AddressingMode): Unit = {
    cpu.P := cpu.stack.popByte()
    cpu.PC := cpu.stack.popWord()
  }

  def RTS(mode: AddressingMode): Unit = {
    cpu.PC := cpu.stack.popWord()
  }

  def SBC(mode: AddressingMode): Unit = {
    val op = cpu.fetchOperand(mode)
    val data = cpu.readOperand(op)

    var result = cpu.A - data

    if(!cpu.getFlag(CpuFlag.Carry))
      result -= 1

    cpu.A := result

    cpu.setFlag(CpuFlag.Carry, result > 0xFFFF)
    cpu.setFlag(CpuFlag.Zero, cpu.A == 0)
    cpu.setFlag(CpuFlag.Overflow, checkOverflow(cpu.A, data, result))
    cpu.setFlag(CpuFlag.Negative, checkNegative(result))
  }

  def SEC(mode: AddressingMode): Unit = {
    cpu.setFlag(CpuFlag.Carry, set = true)
  }

  def SED(mode: AddressingMode): Unit = {
    cpu.setFlag(CpuFlag.Decimal, set = true)
  }

  def SEI(mode: AddressingMode): Unit = {
    cpu.setFlag(CpuFlag.Interrupt, set = true)
  }

  def STA(mode: AddressingMode): Unit = {
    val op = cpu.fetchOperand(mode)
    cpu.writeOperand(op, cpu.A)
  }

  def STX(mode: AddressingMode): Unit = {
    val op = cpu.fetchOperand(mode)
    cpu.writeOperand(op, cpu.X)
  }

  def STY(mode: AddressingMode): Unit = {
    val op = cpu.fetchOperand(mode)
    cpu.writeOperand(op, cpu.Y)
  }

  def TAX(mode: AddressingMode): Unit = {
    cpu.X := cpu.A

    cpu.setFlag(CpuFlag.Zero, cpu.X == 0)
    cpu.setFlag(CpuFlag.Negative, checkNegative(cpu.X))
  }

  def TAY(mode: AddressingMode): Unit = {
    cpu.Y := cpu.A

    cpu.setFlag(CpuFlag.Zero, cpu.Y == 0)
    cpu.setFlag(CpuFlag.Negative, checkNegative(cpu.Y))
  }

  def TSX(mode: AddressingMode): Unit = {
    cpu.X := cpu.stack.SP

    cpu.setFlag(CpuFlag.Zero, cpu.X == 0)
    cpu.setFlag(CpuFlag.Negative, checkNegative(cpu.X))
  }

  def TXA(mode: AddressingMode): Unit = {
    cpu.A := cpu.X

    cpu.setFlag(CpuFlag.Zero, cpu.A == 0)
    cpu.setFlag(CpuFlag.Negative, checkNegative(cpu.A))
  }

  def TXS(mode: AddressingMode): Unit = {
    cpu.stack.SP := cpu.X
  }

  def TYA(mode: AddressingMode): Unit = {
    cpu.A := cpu.Y

    cpu.setFlag(CpuFlag.Zero, cpu.A == 0)
    cpu.setFlag(CpuFlag.Negative, checkNegative(cpu.A))
  }
}
