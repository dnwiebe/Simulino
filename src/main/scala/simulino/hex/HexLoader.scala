package simulino.hex

import java.io.{BufferedReader, Reader}

import simulino.memory.Span

import scala.collection.mutable.ListBuffer

/**
 * Created by dnwiebe on 5/8/15.
 */
class HexLoader {
  var parser = new HexRecordParser ()
  val data = new Array[Byte](1024)
  var lowest = Int.MaxValue
  var highest = Int.MinValue

  def load (rdr: Reader): Unit = {
    val bufrdr = new BufferedReader (rdr)
    var continue = true
    while (continue) {
      val line = bufrdr.readLine ()
      if (line == null) {
        continue = false
      }
      else {
        parser.parse (line) match {
          case None =>
          case Some (span) => applySpan (span)
        }
      }
    }
  }

  def getData (offset: Int, length: Int): Array[Byte] = {
    val array = new Array[Byte] (length)
    (0 until length).foreach {i => array(i) = data(offset + i)}
    array
  }

  private def applySpan (span: Span): Unit = {
    (0 until span.data.length).foreach {i =>
      val address = span.offset + i
      data(address.toInt) = span.data(i)
    }
  }
}
