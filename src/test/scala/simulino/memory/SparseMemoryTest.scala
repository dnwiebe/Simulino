package simulino.memory

import org.scalatest.path

/**
 * Created by dnwiebe on 5/8/15.
 */
//noinspection ZeroIndexToHead
class SparseMemoryTest extends path.FunSpec {

  class AccessibleSparseMemory extends SparseMemory {
    val accessibleSpans = spans
  }

  describe ("A SparseMemory") {
    val subject = new AccessibleSparseMemory ()

    describe ("asked for an arbitrary patch of data") {
      val result = subject.getData (0x4325847394098764L, 4)

      it ("has no spans") {
        assert (subject.accessibleSpans.size === 0)
      }

      it ("returns zeros") {
        val expected = Array(0.toByte, 0.toByte, 0.toByte, 0.toByte)
        (0 until expected.length).foreach {i => assert (result(i) === expected(i))}
        assert (result.length === expected.length)
      }
    }

    describe ("given a single data value") {
      subject.setByte (893745203L, UnsignedByte (42))

      it ("has one span") {
        assert (subject.accessibleSpans(0).offset === 893745203L)
        assert (subject.accessibleSpans(0).data(0) === UnsignedByte(42))
        assert (subject.accessibleSpans(0).data.length === 1)
        assert (subject.accessibleSpans.size === 1)
      }
    }
  }
}
