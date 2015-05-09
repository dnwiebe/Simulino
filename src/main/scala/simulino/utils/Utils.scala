package simulino.utils

object Utils {
  def toHex(number: Int, requestedLength: Int): String = {
    val hex = Integer.toHexString (number).toUpperCase
    val prefix = if (number < 0) "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF" else "00000000000000000000000000000000"
    val composed = prefix + hex
    val realNumberOfDigits = numberOfHexDigits (number)
    composed.substring (composed.length - scala.math.max (realNumberOfDigits, requestedLength))
  }

  def valueIsTooWide(value: Int, bitCount: Int): Boolean = {
    (value >> bitCount) > 0
  }

  def bitRange(startBit: Int, bitCount: Int): String = {
    if (bitCount == 1) {
      "bit " + startBit
    }
    else {
      "bits " + startBit + "-" + (startBit + bitCount - 1)
    }
  }
  
  def join[T] (list: List[T], delimiter: String): String = {
    list match {
      case e if e.isEmpty => ""
      case e if e.length == 1 => e.head.toString
      case e => e.head.toString + delimiter + join (e.tail, delimiter)
    }
  }
  
  def makeMask (startBit: Int, bc: Int): Int = {
    val bitCount = scala.math.min (bc, 32 - startBit)
    if (bitCount == 0) {return 0}
    val allOnes = -1
    val inverseBegun = allOnes << bitCount
    val resultBegun = ~inverseBegun
    val result = resultBegun << startBit
    result
  }
  
  def textToInt (text: String): Int = {
    text match {
      case s if s.startsWith ("0x") => Integer.parseInt (s.substring (2), 16)
      case s => Integer.parseInt (s)
    }
  }

  private def numberOfHexDigits(number: Int): Int = {
    var toShift = number
    var digitCount = 0
    val target = if (number < 0) -1 else 0
    while (toShift != target) {
      digitCount += 1
      toShift = toShift >> 4
    }
    if (digitCount == 0) 1 else digitCount
  }
}