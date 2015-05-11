package simulino.cpu

import simulino.engine.{Engine, Subscriber}
import simulino.simulator.CpuConfiguration

/**
 * Created by dnwiebe on 5/11/15.
 */
trait Cpu extends Subscriber {
  val engine: Engine
  val config: CpuConfiguration
  private var _ip = 0
  private var _sp = 0

  def ip = _ip

  def sp = _sp

  def receive = {
    case c: CpuChange => handle (c)
    case i: Instruction => i.execute (this).foreach (engine.schedule)
    case _ =>
  }

  private def handle (change: CpuChange): Unit = {
    change match {
      case c: IncrementIp => _ip += c.increment
      case c: SetIp => _ip = c.newIp
      case c: IncrementSp => _sp += c.increment
      case c: SetSp => _sp = c.newSp
    }
  }
}
