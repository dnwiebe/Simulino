package simulino.cpu.arch.avr.peripheral

import simulino.cpu.arch.avr.PortHandler
import simulino.utils.Utils._

/**
 * Created by dnwiebe on 6/10/15.
 */
class PinPortHandler (suffix: String, bit: Int) extends PortHandler {
  override val name = s"Port ${suffix}${bit}"
  val DDName = s"DD${suffix}${bit}"
  val PORTName = s"PORT${suffix}${bit}"
  val PINName = s"PIN${suffix}${bit}"
  val chipPinName = s"P${suffix}${bit}"
  override val portNames = List (DDName, PORTName, PINName)

  override def acceptChange (portName: String, oldValue: Int, newValue: Int): Unit = {
    portName match {
      case PORTName => acceptPortChange (oldValue, newValue)
      case DDName => acceptDataDirectionChange (oldValue, newValue)
      case _ => TEST_DRIVE_ME
    }
  }

  private def acceptPortChange (oldValue: Int, newValue: Int): Unit = {
    if (newValue == oldValue) {return}
    showVoltageAtPin (chipPinName, if (newValue == 0) Some (0.0) else Some (5.0))
  }

  private def acceptDataDirectionChange (oldValue: Int, newValue: Int): Unit = {
    if (newValue != 1) {TEST_DRIVE_ME}
  }
}
