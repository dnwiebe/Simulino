package simulino.cpu

import simulino.engine.Event

/**
 * Created by dnwiebe on 5/11/15.
 */
trait CpuChange extends Event {
  val tick = -1L // always immediate
}

case class IncrementIp (increment: Int) extends CpuChange
case class SetIp (newIp: Int) extends CpuChange
case class IncrementSp (increment: Int) extends CpuChange
case class SetSp (newSp: Int) extends CpuChange
