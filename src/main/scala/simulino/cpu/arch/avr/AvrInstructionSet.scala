package simulino.cpu.arch.avr

import simulino.cpu.arch.avr.ATmega._
import simulino.cpu.{InstructionObject, Instruction, InstructionSet}
import simulino.memory.UnsignedByte

/**
 * Created by dnwiebe on 5/15/15.
 */

object AvrInstructionSet {
  private val sesquideciles: Array[List[InstructionObject[_]]] = new Array[List[InstructionObject[_]]] (16)
  initializeSesquideciles ()
  
  private def initializeSesquideciles (): Unit = {
    (0 until 16).foreach {i => sesquideciles(i) = Nil}
    add (0x0, ADD, CPC, MOVW, MULS, NOP, SBC)
    add (0x1, ADC, CP, CPSE)
    add (0x2, AND, EOR)
    add (0x3, CPI)
    add (0x4, SBCI)
    add (0x5, LDD, STD, SUBI)
    add (0x6, ORI)
    add (0x8, LDD, STD)
    add (0x9, ADIW, CLx, LPM, JMP, LDD, LDS, POP, PUSH, RET, RETI, SEx, ST, STD, STS)
    add (0xA, LDD, STD)
    add (0xB, IN, OUT)
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
      case _ => {
        val strBuffer = buffer.mkString(" ")
        val strInstructions = instructions.mkString ("(\"", "\", \"", "\")")
        throw new IllegalStateException (s"Internal error: ambiguous buffer (${strBuffer}): could be any of ${strInstructions}")
      }
    }
  }
}
