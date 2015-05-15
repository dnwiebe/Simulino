package simulino.cpu.arch

import simulino.cpu.Cpu
import simulino.engine.Engine
import simulino.memory.UnsignedByte
import simulino.simulator.CpuConfiguration
import simulino.utils.Utils._

/**
 * Created by dnwiebe on 5/13/15.
 */
class AvrCpu (val engine: Engine, val config: CpuConfiguration) extends Cpu {
  private val registerFile = new RegisterFile ()

  def flag (bit: Symbol): Boolean = TEST_DRIVE_ME
  def register (idx: Int): UnsignedByte = registerFile (idx)
}

class RegisterFile () {
  private val registers = new Array[UnsignedByte] (32)
  (0 until registers.length).foreach {registers(_) = UnsignedByte (0)}

  def apply (idx: Int): UnsignedByte = registers(idx)
}