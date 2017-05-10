/**
  * Created by fcusumano on 5/8/17.
  */
class Stack(val mem: Ram) {
  val SP = Register(Stack.SpStart, "SP", 8)

  def reset(): Unit = {
    SP := Stack.SpStart
  }

  def pushByte(value: Int): Unit = {
    mem.writeByte(Stack.Top | SP, value)
    SP -= 1
  }

  def pushWord(value: Int): Unit = {
    mem.writeWord(Stack.Top | SP, value)
    SP -= 2
  }

  def popByte(): Int = {
    SP += 1
    mem.readByte(SP)
  }

  def popWord(): Int = {
    SP += 2
    mem.readWord(SP)
  }
}

object Stack {
  val Top = 0x0100
  val SpStart = 0xFD
}