package simulino.cpu.arch.avr.ATmega

import simulino.cpu.arch.AvrCpu
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
