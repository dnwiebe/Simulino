package simulino.cpu.arch.ATmega

import simulino.engine.Event

/**
 * Created by dnwiebe on 5/13/15.
 */
case class SetRegister (register: Int, value: Int) extends Event