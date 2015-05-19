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
    matchOpcodePattern (buffer, mask, pattern) match {
      case true => Some (parse (buffer))
      case false => None
    }
  }

  protected def parse (buffer: Array[UnsignedByte]): T

  protected def parseUnsignedParameter (buffer: Array[UnsignedByte], mask: Int): Int = {
    parseParameter (buffer, mask)
  }

  private def parseParameter (buffer: Array[UnsignedByte], mask: Int): Int = {
    var value = bufferToInt (buffer)
    var mutableMask = mask
    var parameter = 0
    while (mutableMask != 0) {
      if ((mutableMask & 0x80000000) != 0) {
        parameter = parameter << 1
        if ((value & 0x80000000) != 0) {
          parameter = parameter | 1
        }
      }
      mutableMask = mutableMask << 1
      value = value << 1
    }
    parameter
  }

  private def matchOpcodePattern (buffer: Array[UnsignedByte], mask: Int, pattern: Int): Boolean = {
    (bufferToInt (buffer) & mask) == pattern
  }

  private def bufferToInt (buffer: Array[UnsignedByte]): Int = {
    List (1, 0, 3, 2).foldLeft (0) {(soFar, i) => i < buffer.length match {
      case true => (soFar << 8) | buffer(i).value
      case false => soFar << 8
    }}
  }
}

trait Instruction[C <: Cpu] extends Event {
  def length: Int
  def latency: Long
  def execute (cpu: C): Seq[Event]
}

object Implicits {

  implicit class RegisterInt (val value: Int) extends AnyVal {
    def bit (parm: Int): Boolean = {
      ((value >> parm) & 0x1) == 1
    }
  }

  implicit class RegisterBit (val value: Boolean) extends AnyVal {
    def ^^ (parm: Boolean): Boolean = (value != parm)
  }
}
