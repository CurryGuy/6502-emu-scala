/**
  * Created by fcusumano on 5/8/17.
  */
class Memory(val size: Int) {
  private val bytes = new Array[Int](size)

  def readByte(position: Int): Int = {
    bytes(position) & 0xFF
  }

  def readWord(position: Int): Int = {
    val l = bytes(position) & 0xFF
    val h = bytes(position + 1) & 0xFF
    (h << 8) | l
  }

  def writeByte(position: Int, value: Int): Unit = {
    bytes(position) = value & 0xFF
  }

  def writeWord(position: Int, value: Int): Unit = {
    bytes(position) = value & 0xFF
    bytes(position + 1) = (value >> 8) & 0xFF
  }

  def clear(): Unit = {
    for(i <- bytes.indices)
      bytes(i) = 0x00
  }

  def mkString(columns: Int): String = {
    var counter = 0
    var result = ""
    for(byte <- bytes) {
      result += "%02X ".format(byte)
      counter += 1
      if(counter >= columns) {
        result += "\n"
        counter = 0
      }
    }
    result
  }
}
