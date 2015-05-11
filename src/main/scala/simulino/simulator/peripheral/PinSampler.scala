package simulino.simulator.peripheral

import simulino.engine.Subscriber
import simulino.simulator.SimulatorConfiguration
import simulino.simulator.events.PinVoltageChange
import simulino.utils.Utils._

import scala.collection.mutable.ListBuffer

/**
 * Created by dnwiebe on 5/10/15.
 */
class PinSampler (val pinNumber: Int, configuration: SimulatorConfiguration) extends Subscriber {

  private case class Change (tick: Long, voltage: Double)

  private val samples = new ListBuffer[Change] ()

  override def receive = {
    case PinVoltageChange (tick, pin, voltage) if pin == pinNumber => addChange (tick, voltage)
    case _ =>
  }

  def sampleAtSecond (second: Double): Double = {
    val tick = (second * configuration.cpu.clockSpeed).toLong
    sampleAtTick (tick)
  }

  def sampleAtTick (tick: Long): Double = {
    (0 until samples.length).find {samples (_).tick <= tick} match {
      case None => 0.0
      case Some (idx) => samples (idx).voltage
    }
  }

  private def addChange (tick: Long, voltage: Double): Unit = {
    (0 until samples.length).find {samples (_).tick <= tick} match {
      case None => samples.append (Change (tick, voltage))
      case Some (idx) => samples.insert (idx, Change (tick, voltage))
    }
  }
}
