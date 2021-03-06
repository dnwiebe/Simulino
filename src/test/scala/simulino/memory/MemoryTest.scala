package simulino.memory

import org.scalatest.path

import scala.util.Try
import simulino.utils.TestUtils._

/**
 * Created by dnwiebe on 5/8/15.
 */
//noinspection ZeroIndexToHead
class MemoryTest extends path.FunSpec {

  describe ("A Memory") {
    val subject = new Memory (2000)

    describe ("directed to addSpan that overlaps") {
      val result = Try (subject.addSpan (Span (1000, new Array[UnsignedByte] (1001))))

      it ("complains") {
        fails (result, new IllegalArgumentException ("Span must end at or before 1999, not 2000"))
      }
    }

    describe ("directed to getData too low") {
      val result = Try (subject.getData (-1, 10))

      it ("complains") {
        fails (result, new IllegalArgumentException ("Data buffer must begin at or after 0, not -1"))
      }
    }

    describe ("directed to getData too high") {
      val result = Try (subject.getData (1991, 10))

      it ("complains") {
        fails (result, new IllegalArgumentException ("Data buffer must end at or before 1999, not 2000"))
      }
    }

    describe ("directed to apply too low") {
      val result = Try (subject (-1))

      it ("complains") {
        fails (result, new IllegalArgumentException ("Address must be between 0 and 1999, not -1"))
      }
    }

    describe ("directed to apply too high") {
      val result = Try (subject (2000))

      it ("complains") {
        fails (result, new IllegalArgumentException ("Address must be between 0 and 1999, not 2000"))
      }
    }

    describe ("directed to update too low") {
      val result = Try (subject (-1) = 0)

      it ("complains") {
        fails (result, new IllegalArgumentException ("Address must be between 0 and 1999, not -1"))
      }
    }

    describe ("directed to update too high") {
      val result = Try (subject (2000) = 0)

      it ("complains") {
        fails (result, new IllegalArgumentException ("Address must be between 0 and 1999, not 2000"))
      }
    }

    describe ("asked for an arbitrary patch of data") {
      val result = subject.getData (1000, 4)

      it ("returns zeros") {
        assert (result === unsignedBytes (0, 0, 0, 0))
      }
    }

    describe ("given a single data value") {
      val address = 1000
      val prev = (subject (address) = 42)

      it ("getData shows the given value surrounded by zeros") {
        assert (subject.getData (address - 2, 5) === unsignedBytes (0, 0, 42, 0, 0))
      }

      it ("apply shows the given value surrounded by zeros") {
        assert (subject(address - 1) === UnsignedByte (0))
        assert (subject(address + 0) === UnsignedByte (42))
        assert (subject(address + 1) === UnsignedByte (0))
      }

      it ("returns the previous value") {
        assert (prev === UnsignedByte (0))
      }
    }

    describe ("given a data buffer") {
      subject.addSpan (Span (1000, unsignedBytes (1, 2, 3, 4)))

      describe ("and another one that overlaps it") {
        subject.addSpan (Span (1002, unsignedBytes (4, 3, 2, 1)))

        it ("getData shows the overlap with the later data winning") {
          assert (subject.getData (999, 8) === unsignedBytes (0, 1, 2, 4, 3, 2, 1, 0))
        }
      }
    }
  }

  describe ("A four-byte Memory") {
    val subject = new Memory (4)

    describe ("when loaded with a four-byte span") {
      subject.addSpan (Span (0, unsignedBytes (100, 101, 102, 103)))

      it ("contains those four bytes") {
        assert (subject.getData (0, 4) === unsignedBytes (100, 101, 102, 103))
      }

      describe ("and overwritten with three two-byte spans") {
        subject.addSpan (Span (0, unsignedBytes (103, 40)))
        subject.addSpan (Span (2, unsignedBytes (40, 100)))
        subject.addSpan (Span (1, unsignedBytes (102, 101)))

        it ("contains four new bytes") {
          assert (subject.getData (0, 4) === unsignedBytes (103, 102, 101, 100))
        }
      }
    }
  }
}
