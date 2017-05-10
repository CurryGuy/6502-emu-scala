package com.nanni.nes

/**
  * Created by fcusumano on 5/9/17.
  */
class Ppu {
  val pixelBuffer = new Array[Int](Ppu.ScreenWidth * Ppu.ScreenHeight)
}

object Ppu {
  val ScreenWidth = 256
  val ScreenHeight = 244
}
