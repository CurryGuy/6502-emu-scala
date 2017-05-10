package com.nanni.nes.cpu

/**
  * Created by fcusumano on 5/8/17.
  */
class Operand(val accumulator: Boolean = false, val address: Int = 0) {
}

object Operand {
  def apply(accumulator: Boolean = false, address: Int = 0):
  Operand = new Operand(accumulator, address)
}