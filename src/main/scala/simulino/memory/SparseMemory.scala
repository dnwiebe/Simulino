package simulino.memory

import scala.collection.mutable.ListBuffer

/**
 * Created by dnwiebe on 5/8/15.
 */
class SparseMemory {

  protected val spans = new ListBuffer[Span] ()

  def addSpan (span: Span): Unit = {
    spans += span
  }

  def getData (offset: Long, length: Int): Array[Byte] = {
    (0 until length).map {i => 0.toByte}.toArray
  }

  def setByte (offset: Long, value: Int): Unit = {
    addSpan (new Span (offset, Array(value.toByte)))
  }
}
