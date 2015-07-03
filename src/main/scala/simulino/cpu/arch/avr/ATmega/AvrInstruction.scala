package simulino.cpu.arch.avr.ATmega

import simulino.cpu.arch.avr.AvrCpu
import simulino.cpu.{Instruction, InstructionObject}
import simulino.memory.UnsignedByte

/**
 * Created by dnwiebe on 5/19/15.
 */
trait AvrInstructionObject[T <: Instruction[AvrCpu]] extends InstructionObject[T] {

  override protected def bufferToInt (buffer: Array[UnsignedByte]): Int = {
    List (1, 0, 3, 2).foldLeft (0) {(soFar, i) => i < buffer.length match {
      case true => (soFar << 8) | buffer(i).value
      case false => soFar << 8
    }}
  }
}

trait ComplexAvrInstructionObject[T <: Instruction[AvrCpu]] extends AvrInstructionObject[T]{
  final override val mask = 0xFFFFFFFF // not used
  final override val pattern = 0xFFFFFFFF // not used
  protected val maskPatternPairs: List[(Int, Int)]

  override def apply (buffer: Array[UnsignedByte]): Option[T] = {
    val matches = maskPatternPairs.flatMap {pair =>
      val (mask, pattern) = pair
      if (matchOpcodePattern (buffer, mask, pattern)) Some (parse (buffer)) else None
    }
    matches match {
      case Nil => None
      case x :: Nil => Some (x)
      case _ => {
        val strBuffer = buffer.mkString(" ")
        val strInstructions = matches.mkString ("(\"", "\", \"", "\")")
        throw new IllegalStateException (s"Internal error: ambiguous buffer (${strBuffer}): could be any of ${strInstructions}")
      }
    }
  }
}

trait AvrInstructionUtils {
  def halfCarry (dest: UnsignedByte, src: UnsignedByte, result: UnsignedByte): Boolean = {
    val nd3 = !(dest bit 3)
    val s3 = src bit 3
    val r3 = result bit 3
    (nd3 && s3) || (s3 && r3) || (r3 && nd3)
  }
}
