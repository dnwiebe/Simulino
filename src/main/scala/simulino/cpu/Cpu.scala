package simulino.cpu

import simulino.engine.{Event, Engine, Subscriber}
import simulino.memory.Memory
import simulino.pinout.Pinout
import simulino.simulator.CpuConfiguration

/**
 * Created by dnwiebe on 5/11/15.
 */
trait Cpu extends Subscriber {
  val engine: Engine
  val config: CpuConfiguration
  val programMemory: Memory
  val dynamicMemory: Memory
  val pinout: Pinout

  def receive = {
    case c: CpuChange => c.execute (this).foreach (engine.schedule)
    case i: Instruction => i.execute (this).foreach (engine.schedule)
    case _ =>
  }
}
