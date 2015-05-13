package simulino.cpu

import simulino.engine.Event
import simulino.memory.UnsignedByte
import simulino.utils.Utils._

/**
 * Created by dnwiebe on 5/11/15.
 */
trait InstructionObject[T <: Instruction[_]] {
  val mask: Int
  val pattern: Int

  def apply (buffer: Array[UnsignedByte]): Option[T] = {
    matchPattern (buffer, mask, pattern) match {
      case true => Some (parse (buffer))
      case false => None
    }
  }

  protected def parse (buffer: Array[UnsignedByte]): T

  private def matchPattern (buffer: Array[UnsignedByte], mask: Int, pattern: Int): Boolean = {
    val value = (0 until 4).foldLeft (0) {(soFar, i) => i < buffer.length match {
      case true => (soFar << 8) | buffer(i)
      case false => soFar << 8
    }}
    (value & mask) == pattern
  }
}

trait Instruction[C <: Cpu] extends Event {
  def length: Int
  def latency: Long
  def execute (cpu: C): Seq[Event]

  implicit protected class RegisterInt (val value: Int) extends AnyVal {
    def bit (parm: Int): Boolean = TEST_DRIVE_ME
  }

  implicit protected class RegisterBit (val value: Boolean) extends AnyVal {
    def dot (parm: Boolean): Boolean = TEST_DRIVE_ME

    def + (parm: Boolean): Boolean = TEST_DRIVE_ME

    def xor (parm: Boolean): Boolean = TEST_DRIVE_ME
  }
}

object Implicits {
}
