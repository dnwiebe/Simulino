package simulino.hex

import java.io.StringReader
import org.scalatest.path
import org.mockito.Mockito._
import simulino.memory.{Memory, Span}
import simulino.utils.TestUtils._

/**
 * Created by dnwiebe on 5/8/15.
 */
class HexLoaderTest extends path.FunSpec {

  describe ("A HexLoader with a mock HexRecordParser") {
    val subject = new HexLoader ()
    val parser = mock (classOf[HexRecordParser])
    subject.parser = parser
    val firstSpan = Span (16, unsignedBytes (12, 13, 14, 15))
    val secondSpan = Span (12, unsignedBytes (8, 9, 10, 11))
    val thirdSpan = Span (128, unsignedBytes (126, 127, -128, -127))
    val fourthSpan = Span (15, unsignedBytes (12, 11))
    when (parser.parse ("Your sports team")).thenReturn (Some (firstSpan))
    when (parser.parse ("Is vastly inferior;")).thenReturn (Some (secondSpan))
    when (parser.parse ("This simple fact is plainly")).thenReturn (Some (thirdSpan))
    when (parser.parse ("Obvious to see!")).thenReturn (Some (fourthSpan))

    val FOUR_LINES = "Your sports team\r\nIs vastly inferior;\nThis simple fact is plainly\r\nObvious to see!"

    describe ("given a four-line Reader") {
      val rdr = new StringReader (FOUR_LINES)
      val memory = mock (classOf[Memory])
      subject.load (rdr, memory)

      it ("applies spans to memory properly") {
        verify (memory).addSpan (firstSpan)
        verify (memory).addSpan (secondSpan)
        verify (memory).addSpan (thirdSpan)
        verify (memory).addSpan (fourthSpan)
      }
    }
  }
}
