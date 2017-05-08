/**
  * Created by fcusumano on 5/8/17.
  */
object AddressingMode extends Enumeration {
  type AddressingMode = AddressingMode.Value

  val Implied = Value
  val Accumulator = Value
  val Immediate = Value
  val ZeroPage = Value
  val ZeroPageX = Value
  val ZeroPageY = Value
  val Relative = Value
  val Absolute = Value
  val AbsoluteX = Value
  val AbsoluteY = Value
  val Indirect = Value
  val IndexedIndirect = Value
  val IndirectIndexed = Value
}
