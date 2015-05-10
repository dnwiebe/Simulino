package simulino.hex

import java.io.{BufferedReader, Reader}

import simulino.memory.{Memory, Span}

import scala.collection.mutable.ListBuffer

/**
 * Created by dnwiebe on 5/8/15.
 */
class HexLoader {
  var parser = new HexRecordParser ()
  val data = new Array[Byte](1024)
  var lowest = Int.MaxValue
  var highest = Int.MinValue

  def load (rdr: Reader, memory: Memory): Unit = {
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
          case Some (span) => memory.addSpan (span)
        }
      }
    }
  }
}
