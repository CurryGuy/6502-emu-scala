package com.nanni.nes

/**
  * Created by Nanni on 08/05/2017.
  */
class Cartridge(bytes: Array[Byte]) {
  if(!bytes.take(4).sameElements(Array('N', 'E', 'S', 0x1A))) {
    throw new Exception("Invalid iNes cartridge!")
  }

  private val prgSize = bytes(5)
  private val chrSize = bytes(6)
  private val mapperNumber = (bytes(6) >> 4) & 0xF
}
