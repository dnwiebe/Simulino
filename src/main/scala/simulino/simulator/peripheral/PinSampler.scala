package simulino.simulator.peripheral

import simulino.engine.Subscriber
import simulino.utils.Utils._

/**
 * Created by dnwiebe on 5/10/15.
 */
class PinSampler (pinNumber: Int) extends Subscriber {

  override def receive = {
    case _ =>
  }

  def sampleAtSecond (second: Double): Double = {
    TEST_DRIVE_ME
    0.0
  }
}
