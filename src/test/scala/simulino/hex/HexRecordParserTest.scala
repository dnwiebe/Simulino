package simulino.hex

import org.scalatest.path

import scala.util.{Success, Failure, Try}

/**
 * Created by dnwiebe on 5/8/15.
 */
class HexRecordParserTest extends path.FunSpec {

  describe ("A HexRecordParser") {
    val subject = new HexRecordParser ()

    describe ("that is fed a null") {
      val result = Try (subject.parse (null))

      it ("complains") {
        fails (result, new NullPointerException ("Line must be provided"))
      }
    }

    describe ("that is fed a blank line") {
      val result = Try (subject.parse (""))

      it ("complains") {
        fails (result, new IllegalArgumentException (".hex record must be at least 11 characters long, not 0"))
      }
    }

    describe ("that is fed a line that doesn't begin with a colon") {
      val result = Try (subject.parse ("No colon at the beginning"))

      it ("complains") {
        fails (result, new IllegalArgumentException (".hex record must begin with a colon, not 'N'"))
      }
    }

    describe ("that is fed a line that begins with a colon but has non-hex characters") {
      val result = Try (subject.parse (":0123456789ABCDEFGH"))

      it ("complains") {
        fails (result, new IllegalArgumentException (".hex record must not contain non-hexadecimal symbols"))
      }
    }

    describe ("that is fed a line with an odd number of hex characters") {
      val result = Try (subject.parse (":0123456789ABCDE"))

      it ("complains") {
        fails (result, new IllegalArgumentException (".hex record must contain an even number of digits, not 15"))
      }
    }

    describe ("that is given a record with a bad checksum") {
      val result = Try (subject.parse (":00000001FE"))

      it ("complains") {
        fails (result, new IllegalArgumentException (".hex record produces a checksum of 255 instead of 0"))
      }
    }

    describe ("that is given a record with a bad byte count") {
      val result = Try (subject.parse (":01000000FF"))

      it ("complains") {
        fails (result, new IllegalArgumentException (".hex record contains 0 data bytes, not 1 as claimed"))
      }
    }

    describe ("that is given a data record") {
      val result = subject.parse (":04123400AABBCC87FE")

      it ("creates the correct Span") {
        val expected = Array(0xAA.toByte, 0xBB.toByte, 0xCC.toByte, 0x87.toByte)
        (0 until expected.length).foreach {i =>
          assert (result.get.data(i) == expected(i))
        }
        assert (result.get.data.length === 4)
        assert (result.get.offset === 0x1234)
      }
    }

    describe ("that is given an EOF record") {
      val result = subject.parse (":00000001FF")

      it ("returns None") {
        assert (result === None)
      }
    }
  }

  private def fails (result: Try[_], exception: Exception): Unit = {
    result match {
      case Success(_) => fail (s"Should have thrown ${exception.getClass.getName} (${exception.getMessage}})")
      case Failure (e) => {
        assert (e.getClass === exception.getClass)
        assert (e.getMessage === exception.getMessage)
      }
    }
  }
}
