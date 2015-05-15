package simulino.cpu.arch.avr.ATmega

import simulino.cpu.CpuChange
import simulino.memory.UnsignedByte

/**
 * Created by dnwiebe on 5/13/15.
 */
case class SetRegister (register: Int, value: UnsignedByte) extends CpuChange

