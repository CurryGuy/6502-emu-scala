package com.nanni.nes.cpu

/**
  * Created by fcusumano on 5/8/17.
  */
class Register(private var _value: Int, val name: String = "TMP", size: Int = 8) {
  private val mask = ~(~0 << size)

  _value &= mask

  def setBit(bit: Int, flag: Boolean): Unit = {
    if(flag) {
      _value |= (1 << bit)
    } else {
      _value &= ~(1 << bit)
    }
  }

  def getBit(bit: Int): Boolean = {
    (_value & (1 << bit)) != 0
  }

  def value: Int = _value

  def getHexString: String = "%02X".format(_value)

  def ==(i: Int): Boolean = _value == i
  def :=(i: Int): Unit = _value = i & mask
  def +=(i: Int): Unit = _value = (_value + i) & mask
  def -=(i: Int): Unit = _value = (_value - i) & mask
  def <<(i: Int): Int = (_value << i) & mask
  def >>(i: Int): Int = (_value >> i) & mask
  def &=(i: Int): Unit = _value = (_value & i) & mask
  def |=(i: Int): Unit = _value = (_value | i) & mask
  def ^=(i: Int): Unit = _value = (_value ^ i) & mask
  def +(i: Int): Register = new Register(_value + i)
  def -(i: Int): Register = new Register(_value - i)
  def &(i: Int): Register = new Register(_value & i)
  def |(i: Int): Register = new Register(_value | i)
  def ^(i: Int): Register = new Register(_value ^ i)
  def ++ : Int = { val s = _value; _value = (_value + 1) & mask; s }
  def -- : Int = { val s = _value; _value = (_value + 1) & mask; s }

  override def toString: String = name + ": " + getHexString
}

object Register {
  def apply(value: Int, name: String, size: Int): Register = new Register(value, name, size)
  def update(newVal: Int, reg: Register): Unit = reg._value = newVal & reg.mask
  implicit def reg2Int(r: Register): Int = r.value
}
