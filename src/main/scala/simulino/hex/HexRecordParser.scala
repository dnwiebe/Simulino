package simulino.hex

import simulino.memory.{UnsignedByte, Span}

/**
 * Created by dnwiebe on 5/8/15.
 */
class HexRecordParser {

  private trait RecordType {
    def toSpanOpt (parser: HexRecordParser, record: HexRecord): Option[Span]

    protected def ensureDataLength (record: HexRecord, requiredLength: Int, recordType: String): Unit = {
      if (record.data.length == requiredLength) {return}
      throw new IllegalArgumentException (s".hex ${recordType} record must have data length of ${requiredLength}, not ${record.data.length}")
    }

    protected def numberFromData (record: HexRecord): Long = {
      record.data.foldLeft (0L) {(soFar, elem) => (soFar * 0x100L) + elem}
    }
  }

  private class EOF extends RecordType {
    override def toSpanOpt (parser: HexRecordParser, record: HexRecord): Option[Span] = {
      None
    }
  }
  private class Data extends RecordType {
    override def toSpanOpt (parser: HexRecordParser, record: HexRecord): Option[Span] = {
      Some (Span (parser.baseAddress + record.address, record.data))
    }
  }

  private class ESA extends RecordType {
    override def toSpanOpt (parser: HexRecordParser, record: HexRecord): Option[Span] = {
      ensureDataLength (record, 2, "ESA")
      parser.baseAddress = (numberFromData (record) << 4).toInt
      None
    }
  }

  private class SA (recordType: String) extends RecordType {
    override def toSpanOpt (parser: HexRecordParser, record: HexRecord): Option[Span] = {
      ensureDataLength (record, 4, recordType)
      parser.startAddress = Some (numberFromData (record))
      None
    }
  }

  private class SSA extends SA ("SSA")

  private class SLA extends SA ("SLA")

  private class ELA extends RecordType {
    override def toSpanOpt (parser: HexRecordParser, record: HexRecord): Option[Span] = {
      ensureDataLength (record, 2, "ELA")
      parser.baseAddress = (numberFromData (record) << 16).toInt
      None
    }
  }

  private case class HexRecord (byteCount: Int, address: Int, recordTypeIndex: Int, data: Array[UnsignedByte], checksum: Int) {
    def recordType: RecordType = indexToRecordType (recordTypeIndex)

    private def indexToRecordType: PartialFunction[Int, RecordType] = {
      case 0 => new Data ()
      case 1 => new EOF ()
      case 2 => new ESA ()
      case 3 => new SSA ()
      case 4 => new ELA ()
      case 5 => new SLA ()
    }
  }

  private var baseAddress = 0
  private var startAddress: Option[Long] = None

  def parse (line: String): Option[Span] = {
    validateLine (line)
    val record = separate (line)
    validateRecord (record)
    record.recordType.toSpanOpt (this, record)
  }

  def getStartAddress: Option[Long] = startAddress

  private val REGEX = ":[0-9A-Fa-f]*".r
  private def validateLine (line: String): Unit = {
    if (line == null) {
      throw new NullPointerException ("Line must be provided")
    }
    if (line.length < 11) {
      throw new IllegalArgumentException (s".hex record must be at least 11 characters long, not ${line.length}")
    }
    if (line(0) != ':') {
      throw new IllegalArgumentException (s".hex record must begin with a colon, not '${line (0)}'")
    }
    line match {
      case REGEX (_*) =>
      case _ => throw new IllegalArgumentException (".hex record must not contain non-hexadecimal symbols")
    }
    if ((line.length & 1) == 0) {
      throw new IllegalArgumentException (s".hex record must contain an even number of digits, not ${line.length - 1}")
    }
  }

  private def separate (line: String): HexRecord = {
    val byteCount = pairToUnsignedByte (line.substring (1, 3))
    val address = extractAddress (line)
    val recordTypeIndex = pairToUnsignedByte (line.substring (7, 9))
    val data = extractData (line)
    val checksum = pairToUnsignedByte (line.substring (line.length - 2))
    new HexRecord (byteCount, address, recordTypeIndex, data, checksum)
  }

  private def validateRecord (record: HexRecord): Unit = {
    if (record.byteCount != record.data.length) {
      throw new IllegalArgumentException (s".hex record contains ${record.data.length} data bytes, not ${record.byteCount} as claimed")
    }
    val checksum = computeChecksum (record)
    if (checksum != 0) {
      throw new IllegalArgumentException (s".hex record produces a checksum of ${checksum} instead of 0")
    }
  }

  private def computeChecksum (record: HexRecord): Int = {
    var checksum = 0
    checksum += record.byteCount
    checksum += record.address >> 8
    checksum += record.address & 0xFF
    checksum += record.recordTypeIndex
    checksum += record.data.foldLeft (0) {(soFar, elem) => soFar + elem}
    checksum += record.checksum
    checksum & 0xFF
  }

  private def extractAddress (line: String): Int = {
    val highByte = pairToUnsignedByte (line.substring (3, 5))
    val lowByte = pairToUnsignedByte (line.substring (5, 7))
    (highByte.value * 0x100) + lowByte
  }

  private def extractData (line: String): Array[UnsignedByte] = {
    val start = 9
    val byteCount = (line.length - start - 2) / 2
    (0 until byteCount).map {i =>
      val offset = start + (i * 2)
      pairToUnsignedByte (line.substring (offset, offset + 2))
    }.toArray
  }

  private def pairToUnsignedByte (pair: String): UnsignedByte = {
    val result = (digit2Int (pair (0)) * 16) + digit2Int (pair (1))
    result
  }

  private def digit2Int: PartialFunction[Char, Int] = {
    case c if (c >= '0') && (c <= '9') => c - '0'
    case c if (c >= 'A') && (c <= 'F') => c - 'A' + 10
    case c if (c >= 'a') && (c <= 'f') => c - 'a' + 10
  }
}
