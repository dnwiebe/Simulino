package simulino.cpu.arch.avr

import simulino.cpu.CpuChange
import simulino.memory.UnsignedByte
import simulino.simulator.peripheral.PinSampler
import simulino.utils.Utils.TEST_DRIVE_ME

/**
 * Created by dnwiebe on 5/19/15.
 */
class RegisterFile () {
  private val registers = new Array[UnsignedByte] (0x200)
  (0 until registers.length).foreach {registers(_) = UnsignedByte (0)}

  def apply (idx: Int): UnsignedByte = registers(idx)
  def update (idx: Int, value: UnsignedByte) {registers(idx) = value}
  def addPinSampler (sampler: PinSampler): Unit = {TEST_DRIVE_ME}

  def handleCpuChange: CpuChange => Unit = {
//    case c: WriteIOSpace => this (c.address + 0x20) = UnsignedByte (c.value)
    case _ => TEST_DRIVE_ME
  }
}
