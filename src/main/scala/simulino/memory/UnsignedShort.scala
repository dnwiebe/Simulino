package simulino.memory

import simulino.utils.Utils

object UnsignedShort {
	implicit def IntToUnsignedShort (input: Int): UnsignedShort = {
	  new UnsignedShort (input)
	}
	
	implicit def UnsignedShortToInt (input: UnsignedShort): Int = {
	  input.value & 0xFFFF
	}
}

class UnsignedShort (proposedValue: Int) {
  def this (high: UnsignedByte, low: UnsignedByte) = this ((high.value << 8) + low.value)
  
  val value = validateProposedValue (proposedValue)
  
  def highByte = new UnsignedByte ((this.value >> 8) & 0xFF)
  def lowByte = new UnsignedByte (this.value & 0xFF)
  
  def == (that: Int): Boolean = {
    value == that
  }

  def + (that: UnsignedShort): UnsignedShort = {
    val sum = this.value + that.value
    new UnsignedShort (sum & 0xFFFF)
  }
  
  override def toString (): String = {
    Utils.toHex (value, 4)
  }
  
  private def validateProposedValue (proposedValue: Int): Int = {
    if (proposedValue > 0xFFFF) {
      throw new IllegalArgumentException ("UnsignedShort is restricted to values between 0x0000 and 0xFFFF, not 0x" +
          Utils.toHex(proposedValue, 4))
    }
    proposedValue & 0xFFFF
  }
}