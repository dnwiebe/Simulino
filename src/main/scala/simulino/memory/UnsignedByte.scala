package simulino.memory

import simulino.utils.Utils

//noinspection LanguageFeature
object UnsignedByte {
  def apply (input: Int): UnsignedByte = new UnsignedByte (input)

	implicit def IntToUnsignedByte (input: Int): UnsignedByte = {
	  new UnsignedByte (input)
	}
	
	implicit def UnsignedByteToInt (input: UnsignedByte): Int = {
	  if ((input.value & 0x80) > 0) {
	    input.value | 0xFFFFFF00
	  }
	  else {
	    input.value
	  }
	}
	
	implicit def UnsignedByteToByte (input: UnsignedByte): Byte = {
	  input.value.asInstanceOf[Byte]
	}
}

class UnsignedByte (proposedValue: Int, val halfCarry: Boolean, 
    val overflow: Boolean, val carry: Boolean) {
  
  val value = validateProposedValue (proposedValue)
  val negative = isNegative (value)
  //noinspection ScalaUnnecessaryParentheses
  val zero = (value == 0)
  
  def this (proposedValue: Int) = this (proposedValue, false, false, false)
  
  def == (that: Int): Boolean = {
    value == that
  }
  
  def == (that: UnsignedByte): Boolean = {
    this.value == that.value
  }
  
  override def equals (o: Any): Boolean = {
    if (o == null) {return false}
    if (o.getClass != classOf[UnsignedByte]) {return false}
    this.== (o.asInstanceOf[UnsignedByte])
  }
  
  def + (that: UnsignedByte): UnsignedByte = {
    val halfCarry = computeAdditionHalfCarry (this.value, that.value)
    val sum = this.value + that.value
    makeFlaggedResult (that, sum, halfCarry)
  }
  
  def - (that: UnsignedByte): UnsignedByte = {
    val halfCarry = computeSubtractionHalfCarry (this.value, that.value)
    val diff = this.value - that.value
    makeFlaggedResult (that, diff, halfCarry)
  }
  
  def | (that: UnsignedByte): UnsignedByte = {
    val result = this.value | that.value
    new UnsignedByte (result & 0xFF, false, false, false)
  }
  
  def ^ (that: UnsignedByte): UnsignedByte = {
    val result = this.value ^ that.value
    new UnsignedByte (result & 0xFF, false, false, false)
  }
  
  override def toString: String = {
    Utils.toHex (value, 2)
  }
  
  private def validateProposedValue (proposedValue: Int): Int = {
    val extendedSign = proposedValue >>> 8
    if ((extendedSign != 0x000000) && (extendedSign != 0xFFFFFF)) {
      throw new IllegalArgumentException ("UnsignedByte is restricted to values between 0 and 255, not " + proposedValue)
    }
    proposedValue & 0xFF
  }
  
  private def computeAdditionHalfCarry (a: Int, b: Int): Boolean = {
    val lowNybbleSum = lowNybble (a) + lowNybble (b)
    (lowNybbleSum & 0x10) > 0
  }
  
  private def computeSubtractionHalfCarry (a: Int, b: Int): Boolean = {
    val lowNybbleDiff = lowNybble (a) - lowNybble (b)
    (lowNybbleDiff & 0x10) > 0
  }
  
  private def makeFlaggedResult (that: UnsignedByte, result: Int, halfCarry: Boolean): UnsignedByte = {
    val carry = (result & 0x100) > 0
    val thisNeg = isNegative (this)
    val thatNeg = isNegative (that)
    val resultNeg = isNegative (result)
    val overflow = if (thisNeg == thatNeg) {resultNeg != thisNeg} else {false}
    new UnsignedByte (result & 0xFF, halfCarry, overflow, carry)
  }
  
  private def lowNybble (value: Int) = value & 0xF
  
  private def isNegative (value: Int) = (value & 0x80) > 0
}