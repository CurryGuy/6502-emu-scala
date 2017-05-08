/**
  * Created by fcusumano on 5/8/17.
  */
object Runner extends App {
  println("Starting...")

  val cpu = new Cpu(new Memory(0x1000), 0x800)

  cpu.mem.writeByte(0x300, 0x10)
  cpu.mem.writeByte(0x800, 0xAD)
  cpu.mem.writeWord(0x801, 0x300)

  cpu.step()

  println(cpu.mem.mkString(50))
  println(cpu.mkString)

  println("Exiting...")
}
