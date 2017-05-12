package com.nanni.nes

/**
  * Created by fcusumano on 5/11/17.
  */
object TvSystem extends Enumeration {
  type TvSystem = TvSystem.Value

  val Ntsc = Value
  val Dual = Value
  val Pal = Value

  def fromValue(value: Int): TvSystem = value & 0xFF match {
    case 0 => Ntsc
    case 1 => Pal
    case _ => Dual
  }
}
