package simulino.hex

import java.io.{InputStreamReader, InputStream, BufferedReader, Reader}

import simulino.memory.{Memory, Span}

import scala.collection.mutable.ListBuffer

/**
 * Created by dnwiebe on 5/8/15.
 */
class HexLoader {
  var parser = new HexRecordParser ()
  val data = new Array[Byte](1024)

  def load (rdr: Reader, memory: Memory): Unit = {
    val bufrdr = new BufferedReader (rdr)
    var continue = true
    var lineNo = 1
    while (continue) {
      val line = bufrdr.readLine ()
      if (line == null) {
        continue = false
      }
      else {
        parser.parse (line, lineNo) match {
          case None =>
          case Some (span) => memory.addSpan (span)
        }
      }
      lineNo += 1
    }
  }

  def load (istr: InputStream, memory: Memory): Unit = {
    load (new InputStreamReader (istr), memory)
  }
}
