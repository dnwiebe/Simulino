package simulino.cpu

import simulino.memory.UnsignedByte

/**
 * Created by dnwiebe on 5/15/15.
 */
trait InstructionSet {

  def apply (memory: Array[UnsignedByte]): Option[Instruction[_]]
}
