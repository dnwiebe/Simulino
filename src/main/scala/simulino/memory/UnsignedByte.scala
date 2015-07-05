package simulino.memory

import simulino.utils.Utils
import simulino.utils.Utils._

//noinspection LanguageFeature
object UnsignedByte {
  def apply (input: Int): UnsignedByte = new UnsignedByte (input)

	implicit def IntToUnsignedByte (input: Int): UnsignedByte = {
	  new UnsignedByte (input)
	}
	
	implicit def UnsignedByteToInt (input: UnsignedByte): Int = {
    input.value
	}
	
	implicit def UnsignedByteToByte (input: UnsignedByte): Byte = {
	  input.value.asInstanceOf[Byte]
	}
}

class UnsignedByte (proposedValue: Int) {
  
  val value = validateProposedValue (proposedValue)
  val signedValue = if ((value & 0x80) > 0) 0xFFFFFF00 | value else value

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
    UnsignedByte ((this.value + that.value) & 0xFF)
  }
  
  def - (that: UnsignedByte): UnsignedByte = {
    UnsignedByte ((this.value - that.value) & 0xFF)
  }

  def ^ (that: UnsignedByte): UnsignedByte = {
    val result = this.value ^ that.value
    new UnsignedByte (result & 0xFF)
  }

  def << (that: Int): Int = {
    (value & 0xFF) << that
  }

  def >> (that: Int): Int = {
    (value & 0xFF) >> that
  }

  def bit (idx: Integer): Boolean = {
    (value >> idx) & 1 match {
      case 0 => false
      case _ => true
    }
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
}