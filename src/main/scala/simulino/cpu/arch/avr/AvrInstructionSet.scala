package simulino.cpu.arch.avr

import simulino.cpu.arch.avr.ATmega._
import simulino.cpu.{InstructionObject, Instruction, InstructionSet}
import simulino.memory.UnsignedByte

/**
 * Created by dnwiebe on 5/15/15.
 */

object AvrInstructionSet {
  private val sesquideciles: Array[List[InstructionObject[_]]] = new Array[List[InstructionObject[_]]] (16)
  initializeSesquideciles
  
  private def initializeSesquideciles: Unit = {
    (0 until 16).foreach {i => sesquideciles(i) = Nil}
    add (0x0, ADD, CPC, MULS, NOP, SBC)
    add (0x1, CP, CPSE)
    add (0x2, EOR)
    add (0x3, CPI)
    add (0x9, JMP /* Not in all AVR instruction sets */, ST, SEx)
    add (0xB, OUT)
    add (0xC, RJMP)
    add (0xD, RCALL)
    add (0xE, LDI)
    add (0xF, BRBC)
  }
  
  private def add (sesquidecile: Int, instructions: InstructionObject[_]*): Unit = {
    instructions.foreach {instruction => sesquideciles(sesquidecile) = instruction :: sesquideciles(sesquidecile)}
  }
}

class AvrInstructionSet extends InstructionSet[AvrCpu] {
  import AvrInstructionSet._
  
  override def apply (buffer: Array[UnsignedByte]): Option[Instruction[AvrCpu]] = {
    val sesquidecile = buffer(1).value >> 4
    val instructions: Seq[Instruction[AvrCpu]] = sesquideciles(sesquidecile).flatMap {instObj =>
      instObj(buffer).asInstanceOf[Option[Instruction[AvrCpu]]]
    }
    instructions match {
      case Nil => None
      case inst :: Nil => Some (inst)
      case _ => throw new IllegalStateException (s"Internal error: ambiguous buffer ${buffer}")
    }
  }
}
