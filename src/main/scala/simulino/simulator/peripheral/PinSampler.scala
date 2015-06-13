package simulino.simulator.peripheral

import simulino.engine.Subscriber
import simulino.simulator.SimulatorConfiguration
import simulino.simulator.events.PinVoltageChange
import simulino.utils.Utils._

import scala.collection.mutable.ListBuffer

/**
 * Created by dnwiebe on 5/10/15.
 */
class PinSampler (val clockSpeed: Int) {

  private case class Change (tick: Long, voltage: Option[Double])
  private val samples = new ListBuffer[Change] ()

  def sampleAtSecond (second: Double): Option[Double] = {
    val tick = (second * clockSpeed).toLong
    sampleAtTick (tick)
  }

  def sampleAtTick (tick: Long): Option[Double] = {
    (0 until samples.length).find {samples (_).tick <= tick} match {
      case None => None
      case Some (idx) => samples (idx).voltage
    }
  }

  def addSample (tick: Long, voltage: Option[Double]): Unit = {
    (0 until samples.length).find {samples (_).tick <= tick} match {
      case None => samples.append (Change (tick, voltage))
      case Some (idx) => samples.insert (idx, Change (tick, voltage))
    }
  }
}
