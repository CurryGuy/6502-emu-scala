import AddressingMode.AddressingMode
import CpuFlag.CpuFlag

/**
  * Created by fcusumano on 5/8/17.
  */
class Cpu(val mem: Memory, programStart: Int) {
  val StackTop = 0x01FF

  val stack = new Stack(mem, StackTop)
  val instructions = new Instructions(this)

  val A  = Register(0, "A", 8)
  val X  = Register(0, "X", 8)
  val Y  = Register(0, "Y", 8)
  val P  = Register(0, "P", 8)
  val PC = Register(programStart, "PC", 16)

  private var _cycles = 0

  def cycles : Int = _cycles

  def getFlag(flag: CpuFlag): Boolean = P.getBit(flag.id)
  def setFlag(flag: CpuFlag, set: Boolean): Unit = P.setBit(flag.id, set)
  def incrementCycles(amount: Int): Unit = _cycles += amount

  def reset(): Unit = {
    A := 0
    X := 0
    Y := 0
    P := 0

    PC := programStart

    stack.reset()
    mem.clear()
  }

  def step(): Unit = {
    val opcode = mem.readByte(PC)
    PC += 1

    val instruction = instructions.getInstruction(opcode)

    if(instruction.isDefined)
      instruction.get.run()
    else
      throw new Exception(s"Invalid opcode: $opcode")
  }

  def fetchOperand(mode: AddressingMode): Operand = mode match {
    case AddressingMode.Accumulator => Operand(accumulator = true)
    case AddressingMode.Immediate =>
      val op = Operand(address = PC)
      PC += 1
      op

    case AddressingMode.ZeroPage =>
      val addr = mem.readByte(PC)
      PC += 1
      Operand(address = addr)

    case AddressingMode.ZeroPageX =>
      val addr = (mem.readByte(PC) + X) & 0xFF
      PC += 1
      Operand(address = addr)

    case AddressingMode.ZeroPageY =>
      val addr = (mem.readByte(PC) + Y) & 0xFF
      PC += 1
      Operand(address = addr)

    case AddressingMode.Relative =>
      val addr = mem.readByte(PC).toByte.toInt
      PC += 1
      Operand(address = addr)

    case AddressingMode.Absolute =>
      val addr = mem.readWord(PC)
      PC += 2
      Operand(address = addr)

    case AddressingMode.AbsoluteX =>
      var addr = mem.readWord(PC) + X
      if(addr > 0xFFFF) {
        incrementCycles(1)
        addr &= 0xFFFF
      }
      PC += 2
      Operand(address = addr)

    case AddressingMode.AbsoluteY =>
      var addr = mem.readWord(PC) + Y
      if(addr > 0xFFFF) {
        incrementCycles(1)
        addr &= 0xFFFF
      }
      PC += 2
      Operand(address = addr)

    case AddressingMode.Indirect =>
      val lbAddr = mem.readWord(PC)
      val addr = mem.readWord(lbAddr)
      PC += 2
      Operand(address = addr)

    case AddressingMode.IndexedIndirect =>
      val zeroPageAddr = (mem.readByte(PC) + X) & 0xFF
      PC += 1
      val addr = mem.readWord(zeroPageAddr)
      Operand(address = addr)

    case AddressingMode.IndirectIndexed =>
      val zeroPageAddr = mem.readByte(PC)
      PC += 1
      var addr = mem.readWord(zeroPageAddr) + Y
      if(addr > 0xFFFF) {
        incrementCycles(1)
        addr &= 0xFFFF
      }
      Operand(address = addr)

    case _ => Operand(address = -1)
  }

  def readOperand(operand: Operand): Int = {
    if(operand.accumulator)
      A.toInt
    else if(operand.address >= 0)
      mem.readByte(operand.address)
    else 0
  }

  def writeOperand(operand: Operand, data: Int): Unit = {
    if(operand.accumulator)
      A := data
    else if(operand.address >= 0)
      mem.writeByte(operand.address, data)
  }

  def mkString : String = {
    s"${stack.sp} $PC $A $X $Y"
  }
}
