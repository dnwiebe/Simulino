package simulino.cpu

import simulino.engine.Event
import simulino.utils.Utils._

/**
 * Created by dnwiebe on 5/11/15.
 */
trait CpuChange[C <: Cpu] extends Event {
  def mods (cpu: C): String
}

case class IncrementIp (increment: Int) extends CpuChange[Cpu] {
  override def mods (cpu: Cpu): String = {
    val before = cpu.ip
    val after = before + increment
    s"IP: $$${toHex (before, 2)} -> $$${toHex (after, 2)}"
  }
}

case class SetIp (newIp: Int) extends CpuChange[Cpu] {
  override def mods (cpu: Cpu): String = {
    val before = cpu.ip
    s"IP: $$${toHex (before, 2)} -> $$${toHex (newIp, 2)}"
  }
}

case class ScheduleNextInstruction () extends CpuChange[Cpu] {
  override def mods (cpu: Cpu): String = "" //  TODO: Maybe this shouldn't be a CpuChange.
}
