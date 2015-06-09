package simulino.utils

import com.fasterxml.jackson.databind.JsonNode

object Utils {
  def toHex(number: Int, requestedLength: Int): String = {
    val hex = Integer.toHexString (number).toUpperCase
    val prefix = if (number < 0) "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF" else "00000000000000000000000000000000"
    val composed = prefix + hex
    val realNumberOfDigits = numberOfHexDigits (number)
    composed.substring (composed.length - scala.math.max (realNumberOfDigits, requestedLength))
  }

  private val hexZeroX = "0x([0-9A-Fa-f]+)".r
  private val hexNoPrefix = "([0-9A-Fa-f]+)".r
  def fromHex (hex: String): Int = {
    val hexString = hex match {
      case hexZeroX (s) => s
      case hexNoPrefix (s) => s
      case _ => throw new IllegalArgumentException (s"'${hex}' cannot be converted from hex")
    }
    val digits = "0123456789ABCDEF"
    hexString.foldLeft (0) {(soFar, c) =>
      (soFar << 4) | digits.indexOf (c.toUpper)
    }
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

  def intFromBytes (bytes: Int*): Int = {
    bytes.foldLeft (0) {(soFar, byte) => (soFar << 8) | (byte & 0xFF)}
  }

  def hexOrDec (node: JsonNode): Int = {
    if (node.isTextual) fromHex (node.asText ()) else node.asInt ()
  }

  def TEST_DRIVE_ME: Nothing = throw new UnsupportedOperationException ("Test-drive me!")

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