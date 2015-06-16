package simulino.cpu.arch.avr.peripheral

import simulino.cpu.arch.avr.{AvrCpu, PortHandler}
import simulino.engine.TickSink
import simulino.utils.Utils._

/**
 * Created by dnwiebe on 5/25/15.
 */
class TimerCounter0Handler extends PortHandler with TickSink {
  override val name = s"Timer/Counter 0"
  override val portNames = List ("WGM02", "WGM0", "COM0A", "COM0B", "TCNT0", "TOV0", "OCR0A", "TOIE0",
    "OCIE0A", "OCIE0B", "CS0", "FOC0A", "FOC0B")
  private var cpu: AvrCpu = null

  override def initialize (cpu: AvrCpu) {
    super.initialize (cpu)
    this.cpu = cpu
    cpu.engine.removeTickSink(this)
  }

  override def tick (count: Long): Unit = {
    if (readFromPort ("OCIE0A") > 0) {TEST_DRIVE_ME}
    if (readFromPort ("OCIE0B") > 0) {TEST_DRIVE_ME}
    if (readFromPort ("COM0A") > 0) {TEST_DRIVE_ME}
    if (readFromPort ("COM0B") > 0) {TEST_DRIVE_ME}
    val newCounter = WGM match {
      case 0 => normalMode ()
      case 2 => clearTimerOnCompareMatchMode ()
      case 3 => fastPwmMode (false)
      case 7 => fastPwmMode (true)
      case x => println (s"WGM is ${x}"); TEST_DRIVE_ME
    }
    writeToPort ("TCNT0", newCounter)
  }

  override def acceptChange (portName: String, oldValue: Int, newValue: Int): Unit = {
    portName match {
      case "TOV0" => clearWithOne (portName, newValue)
      case "TOIE0" => // TODO
      case "CS0" => setClockSource (newValue)
      case "COM0A" => // TODO
      case "COM0B" => // TODO
      case "FOC0A" => // TODO
      case "FOC0B" => // TODO
      case "OCIE0A" => // TODO
      case "OCIE0B" => // TODO

      case "WGM0" =>
      case "WGM02" =>
      case _ => println (s"\n\nChange to unimplemented port: ${portName}\n\n"); TEST_DRIVE_ME
    }
  }

  def WGM = (readFromPort ("WGM02") << 2) | readFromPort ("WGM0")

  private def normalMode (): Int = {
    val newCounter = (readFromPort ("TCNT0") + 1) & 0xFF
    if (newCounter == 0) {
      strobe ()
    }
    newCounter
  }

  private def clearTimerOnCompareMatchMode (): Int = {
    var newCounter = readFromPort ("TCNT0") + 1
    if (newCounter == readFromPort ("OCR0A") + 1) {
      newCounter = 0
      strobe ()
    }
    newCounter
  }

  private def fastPwmMode (useCompareRegister: Boolean): Int = {
    var newCounter = readFromPort ("TCNT0") + 1
    val limit = if (useCompareRegister) {readFromPort ("OCR0A")} else 0xFF
    if (newCounter == limit + 1) {
      newCounter = 0
      strobe ()
    }
    newCounter
  }

  private def strobe (): Unit = {
    writeToPort ("TOV0", 1)
    if (readFromPort ("TOIE0") > 0) {raiseInterrupt ("TIM0_OVF")}
  }

  private def clearWithOne (portName: String, newValue: Int): Unit = {
    if (newValue != 1) {TEST_DRIVE_ME}
    writeToPort (portName, 0)
  }

  private def setClockSource (cs: Int): Unit = {
    cs match {
      case 0 => cpu.engine.removeTickSink (this); cpu.prescaler.removeSubscriber (this)
      case 1 => cpu.engine.addTickSink (this); cpu.prescaler.removeSubscriber (this)
      case 2 => cpu.engine.removeTickSink (this); cpu.prescaler.addSubscriber (this, 8)
      case 3 => cpu.engine.removeTickSink (this); cpu.prescaler.addSubscriber (this, 64)
      case 4 => cpu.engine.removeTickSink (this); cpu.prescaler.addSubscriber (this, 256)
      case 5 => cpu.engine.removeTickSink (this); cpu.prescaler.addSubscriber (this, 1024)
      case x => println (s"External clock source (CS0 = ${x}) not yet supported"); TEST_DRIVE_ME
    }
  }
}
