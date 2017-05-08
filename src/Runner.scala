/**
  * Created by fcusumano on 5/8/17.
  */
object Runner extends App {
  println("Starting...")

  val cpu = new Cpu(new Memory(0x10000), 0x800)

  var c = 0
  for(i <- Array(0xA9, 0x10, 0x8D, 0xB8, 0x0B)) {
    cpu.mem.writeByte(0x800 + c, i)
    c += 1
  }

  cpu.step()
  cpu.step()
  cpu.step()
  cpu.step()
  cpu.step()

  println(cpu.mkString)
  println(cpu.mem.readByte(3000))

  println("Exiting...")
}
