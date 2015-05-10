package simulino.utils

import scala.util.{Failure, Success, Try}
import org.scalatest.Assertions._

/**
 * Created by dnwiebe on 5/10/15.
 */
object TestUtils {
  def fails (result: Try[_], exception: Exception): Unit = {
    result match {
      case Success (_) => fail (s"Should have thrown ${exception.getClass.getName} (${exception.getMessage}})")
      case Failure (e) => {
        assert (e.getClass === exception.getClass)
        assert (e.getMessage === exception.getMessage)
      }
    }
  }
}
