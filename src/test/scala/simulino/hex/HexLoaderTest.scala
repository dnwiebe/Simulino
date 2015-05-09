package simulino.hex

import java.io.StringReader
import org.scalatest.path
import org.mockito.Mockito._

/**
 * Created by dnwiebe on 5/8/15.
 */
class HexLoaderTest extends path.FunSpec {

  describe ("A HexLoader with a mock HexRecordParser") {
    val subject = new HexLoader ()
    val parser = mock (classOf[HexRecordParser])
    subject.parser = parser
    val firstSpan = Span (16, Array(12.toByte, 13.toByte, 14.toByte, 15.toByte))
    val secondSpan = Span (12, Array(8.toByte, 9.toByte, 10.toByte, 11.toByte))
    val thirdSpan = Span (128, Array(126.toByte, 127.toByte, -128.toByte, -127.toByte))
    val fourthSpan = Span (15, Array(12.toByte, 11.toByte))
    when (parser.parse ("Your sports team")).thenReturn (Some (firstSpan))
    when (parser.parse ("Is vastly inferior;")).thenReturn (Some (secondSpan))
    when (parser.parse ("This simple fact is plainly")).thenReturn (Some (thirdSpan))
    when (parser.parse ("Obvious to see!")).thenReturn (Some (fourthSpan))

    val FOUR_LINES = "Your sports team\r\nIs vastly inferior;\nThis simple fact is plainly\r\nObvious to see!"

    describe ("given a four-line Reader") {
      val rdr = new StringReader (FOUR_LINES)
      subject.load (rdr)

      it ("shows zeros in 0-11") {
        assert (subject.getData (0, 12) === (0 until 12).map {i => 0.toByte}.toArray)
      }

      it ("shows the correct data in 12-19") {
        assert (subject.getData (12, 8) === Array (
          8.toByte, 9.toByte, 10.toByte, 12.toByte,
          11.toByte, 13.toByte, 14.toByte, 15.toByte
        ))
      }

      it ("shows zeros in 20-23") {
        assert (subject.getData (20, 4) === (0 until 4).map {i => 0.toByte}.toArray)
      }

      it ("shows the right thing up around 128") {
        assert (subject.getData (127, 6) === Array (
          0.toByte, 126.toByte, 127.toByte, -128.toByte, -127.toByte, 0.toByte
        ))
      }

      it ("knows the lowest address seen") {
        assert (subject.lowestAddressSet === 12)
      }

      it ("knows the highest address seen") {
        assert (subject.highestAddressSet === 131)
      }
    }
  }
}
