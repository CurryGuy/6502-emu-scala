/**
  * Created by fcusumano on 5/8/17.
  */
class Stack(val mem: Memory, val top: Int) {
  val sp = Register(top, "SP", 16)

  def reset(): Unit = {
    sp := top
  }

  def pushByte(value: Int): Unit = {
    mem.writeByte(sp, value)
    sp -= 1
  }

  def pushWord(value: Int): Unit = {
    mem.writeWord(sp, value)
    sp -= 2
  }

  def popByte(): Int = {
    val byte = mem.readByte(sp)
    sp += 1
    byte
  }

  def popWord(): Int = {
    val word = mem.readWord(sp)
    sp += 2
    word
  }
}
