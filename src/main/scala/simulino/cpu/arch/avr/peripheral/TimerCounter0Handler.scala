package simulino.cpu.arch.avr.peripheral

import simulino.cpu.arch.avr.PortHandler
import simulino.engine.TickSink
import simulino.utils.Utils._

/**
 * Created by dnwiebe on 5/25/15.
 */
class TimerCounter0Handler extends PortHandler with TickSink {
  override val name = s"Timer/Counter 0"
  override val portNames = List ()

  def tick (count: Long): Unit = {

  }
}
