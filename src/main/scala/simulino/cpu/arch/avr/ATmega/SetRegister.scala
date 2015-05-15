package simulino.cpu.arch.avr.ATmega

import simulino.engine.Event
import simulino.memory.UnsignedByte

/**
 * Created by dnwiebe on 5/13/15.
 */
case class SetRegister (register: Int, value: UnsignedByte) extends Event
