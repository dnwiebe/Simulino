package simulino.memory

/**
 * Created by dnwiebe on 5/8/15.
 */
case class Span (offset: Int, data: Array[UnsignedByte]) {
  if (offset < 0) {throw new IllegalArgumentException (s"Span offset must be nonnegative, not ${offset}")}

  def length = data.length
  def end = offset + length

  def contains (idx: Int): Boolean = {
    (idx >= offset) && (idx < end)
  }

  def apply (idx: Int): UnsignedByte = {
    contains (idx) match {
      case true => data (idx - offset)
      case false => UnsignedByte (0)
    }
  }

  def update (idx: Int, value: UnsignedByte): Unit = {
    contains (idx) match {
      case true => data (idx - offset) = value
      case false =>
    }
  }
}
