package com.nanni.nes.cpu

/**
  * Created by fcusumano on 5/9/17.
  */
object InterruptType extends Enumeration {
  type InterruptType = InterruptType.Value
  val None = Value
  val Nmi = Value
  val Irq = Value
  val Reset = Value
  val Break = Value
}
