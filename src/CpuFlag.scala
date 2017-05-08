/**
  * Created by fcusumano on 5/8/17.
  */
object CpuFlag extends Enumeration {
  type CpuFlag = CpuFlag.Value

  val Carry = Value(1 << 0)
  val Zero = Value(1 << 1)
  val Interrupt = Value(1 << 2)
  val Decimal = Value(1 << 3)
  val Break = Value(1 << 4)
  val Overflow = Value(1 << 5)
  val Negative = Value(1 << 6)
}