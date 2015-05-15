package simulino.cpu.arch

import java.util

import simulino.cpu.Cpu
import simulino.cpu.arch.avr.ATmega.Flag
import simulino.cpu.arch.avr.AvrInstructionSet
import simulino.engine.Engine
import simulino.memory.UnsignedByte
import simulino.simulator.CpuConfiguration
import simulino.utils.Utils._

/**
 * Created by dnwiebe on 5/13/15.
 */
class AvrCpu (val engine: Engine, val config: CpuConfiguration) extends Cpu {
  private val statusRegister = new StatusRegister ()
  private val registerFile = new RegisterFile ()
  private val _instructionSet = new AvrInstructionSet ()

  def flag (flag: Flag): Boolean = statusRegister (flag)
  def register (idx: Int): UnsignedByte = registerFile (idx)
  def instructionSet = _instructionSet
}

class StatusRegister () {
  private val flags = new util.EnumMap[Flag, Boolean] (classOf[Flag])

  def apply (flag: Flag): Boolean = flags.get (flag)
}

class RegisterFile () {
  private val registers = new Array[UnsignedByte] (32)
  (0 until registers.length).foreach {registers(_) = UnsignedByte (0)}

  def apply (idx: Int): UnsignedByte = registers(idx)
}