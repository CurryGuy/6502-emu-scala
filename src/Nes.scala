/**
  * Created by fcusumano on 5/9/17.
  */
class Nes {
  val memory = new Ram()
  val cpu = new Cpu(memory)
}
