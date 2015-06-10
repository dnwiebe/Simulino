package simulino.memory

/**
 * Created by dnwiebe on 5/8/15.
 */

class Memory (size: Int) {
  private val data = new Array[Byte] (size)

  def addSpan (span: Span): Unit = {
    hiValidate (span.end - 1, s"Span must end at or before ${size - 1}")
    (span.offset until span.end).foreach {i => data(i) = span(i).value.toByte}
  }

  def getData (address: Int, length: Int): Array[UnsignedByte] = {
    loValidate (address, "Data buffer must begin at or after 0")
    hiValidate (address + length - 1, s"Data buffer must end at or before ${size - 1}")
    val ubs = new Array[UnsignedByte] (length)
    (0 until length).foreach {i => ubs(i) = UnsignedByte (data (address + i))}
    ubs
  }

  def apply (address: Int): UnsignedByte = {
    validate (address, s"Address must be between 0 and ${size - 1}")
    UnsignedByte (data (address))
  }

  def update (address: Int, value: UnsignedByte): UnsignedByte = {
    validate (address, s"Address must be between 0 and ${size - 1}")
    val prev = data (address)
    data (address) = value
    UnsignedByte (prev)
  }

  def update (address: Int, value: Int): UnsignedByte = {
    update (address, UnsignedByte (value))
  }

  private def validate (address: Int, msg: String): Unit = {
    loValidate (address, msg)
    hiValidate (address, msg)
  }

  private def loValidate (address: Int, msg: String): Unit = {
    if (address < 0) {
      throw new IllegalArgumentException (s"${msg}, not ${address}")
    }
  }

  private def hiValidate (address: Int, msg: String): Unit = {
    if (address >= size) {
      throw new IllegalArgumentException (s"${msg}, not ${address}")
    }
  }
}
