package com.nanni.nes

import com.nanni.nes.mappers.Mapper
import com.nanni.nes.ppu.Ppu

/**
  * Created by fcusumano on 5/8/17.
  */
class Memory(val mapper: Mapper, val ppu: Ppu) {
  private val bytes = new Array[Int](Memory.Size)

  def readByte(position: Int): Int = {
    if(position > 0x4018) {
      mapper.readByte(position)
    } else if(position >= 0x1FFF) {
      bytes(position & 0x7FFF)
    } else if(position <= 0x3FFF) {
      ppu.readRegister(position & 0x7)
    } else {
      position >> 8
    }
  }

  def readWord(position: Int): Int = {
    val l = bytes(position) & 0xFF
    val h = bytes(position + 1) & 0xFF
    (h << 8) | l
  }

  def writeByte(position: Int, value: Int): Unit = {
    if(position > 0x4018) {
    }
  }

  def writeWord(position: Int, value: Int): Unit = {
    bytes(position) = value & 0xFF
    bytes(position + 1) = (value >> 8) & 0xFF
  }

  def clear(): Unit = {
    for(i <- bytes.indices)
      bytes(i) = 0x00
  }

  def init(): Unit = {
    for(i <- bytes.indices)
      bytes(i) = 0xFF

    writeByte(0x0008, 0xF7)
    writeByte(0x0009, 0xEF)
    writeByte(0x000A, 0xDF)
    writeByte(0x000F, 0xBF)

    for(i <- 0x4000 to 0x400F)
      writeByte(i, 0x00)

    writeByte(0x4015, 0x00)
    writeByte(0x4017, 0x00)
  }

  def reset(): Unit = {
    writeByte(0x4015, 0x00)
  }

  def mkString(columns: Int): String = {
    var counter = 0
    var result = ""
    for(byte <- bytes) {
      result += "%02X ".format(byte)
      counter += 1
      if(counter >= columns) {
        result += "\n"
        counter = 0
      }
    }
    result
  }
}

object Memory {
  val Size = 0x10000
}