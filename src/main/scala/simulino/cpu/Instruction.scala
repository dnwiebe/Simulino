package simulino.cpu

import simulino.engine.Event

/**
 * Created by dnwiebe on 5/11/15.
 */
trait Instruction extends Event {
  def execute (cpu: Cpu): Seq[Event]
}
