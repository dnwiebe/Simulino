package simulino.cpu.arch.avr

import simulino.cpu.CpuChange

/**
 * Created by dnwiebe on 5/19/15.
 */

case class WriteIOSpace (address: Int, value: Int) extends CpuChange
