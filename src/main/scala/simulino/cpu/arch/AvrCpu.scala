package simulino.cpu.arch

import simulino.cpu.Cpu
import simulino.engine.Engine
import simulino.simulator.CpuConfiguration
import simulino.utils.Utils._

/**
 * Created by dnwiebe on 5/13/15.
 */
class AvrCpu (val engine: Engine, val config: CpuConfiguration) extends Cpu {
  def register (idx: Int): Int = TEST_DRIVE_ME
}
