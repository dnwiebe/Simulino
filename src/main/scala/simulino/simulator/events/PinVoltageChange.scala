package simulino.simulator.events

import simulino.engine.Event

/**
 * Created by dnwiebe on 5/11/15.
 */
case class PinVoltageChange  (val tick: Long, val pin: Int, val voltage: Double) extends Event
