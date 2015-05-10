package simulino.memory

import org.scalatest.path

import scala.util.Try
import simulino.utils.TestUtils._

/**
 * Created by dnwiebe on 5/10/15.
 */
class SpanTest extends path.FunSpec {
  describe ("Creating a Span with a negative offset") {
    val result = Try (Span (-1, new Array[UnsignedByte] (0)))

    it ("doesn't work") {
      fails (result, new IllegalArgumentException ("Span offset must be nonnegative, not -1"))
    }
  }
}
