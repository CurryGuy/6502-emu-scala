/**
  * Created by fcusumano on 5/8/17.
  */
object Runner extends App {
  println("Starting...")

  val cpu = new Cpu(new Memory(0x10000), 0x3000)

  cpu.mem.writeByte(0x3000, 0xE8)
  cpu.mem.writeByte(0x3001, 0xC8)
  cpu.mem.writeByte(0x3002, 0xC8)
  cpu.mem.writeByte(0x3003, 0xC8)

  cpu.step()
  cpu.step()
  cpu.step()
  cpu.step()

  println(cpu.mkString)

  println("Exiting...")
}
