package com.nanni.nes.ppu

/**
  * Created by fcusumano on 5/9/17.
  */
class Ppu {
  val pixelBuffer = new Array[Int](Ppu.ScreenWidth * Ppu.ScreenHeight)

  def readRegister(address: Int): Int = {
    0
  }
}

object Ppu {
  val ScreenWidth = 256
  val ScreenHeight = 244
}
