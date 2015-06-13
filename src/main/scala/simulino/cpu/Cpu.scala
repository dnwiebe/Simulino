package simulino.cpu

import simulino.cpu.arch.avr.AvrCpu
import simulino.engine.{Event, Engine, Subscriber}
import simulino.memory.{UnsignedByte, Memory}
import simulino.simulator.{ExecutionLog, CpuConfiguration}
import simulino.simulator.peripheral.PinSampler
import simulino.utils.Utils._

/**
 * Created by dnwiebe on 5/11/15.
 */
trait Cpu extends Subscriber {
  val engine: Engine
  val config: CpuConfiguration
  val programMemory: Memory
  val instructionSet: InstructionSet[_]
  var pinSamplers = Map[String, PinSampler] ()

  private var _ip = 0

  def ip: Int = _ip
  protected def ip_= (newIp: Int): Unit = {
    _ip = newIp
  }

  override def receive: PartialFunction[Event, Unit] = {
    class PFC[C <: Cpu] extends PartialFunction[Event, Unit] {
      override def isDefinedAt (event: Event): Boolean = {
        (event.getClass == classOf[CpuChange[_]]) || (event.getClass == classOf[Instruction[_]])
      }
      override def apply (event: Event): Unit = {
        event match {
          case c: CpuChange[C] => handleCpuChange (c)
          case i: Instruction[C] => handleInstruction (i.asInstanceOf[Instruction[Cpu]])
          case x => throw new UnsupportedOperationException (s"${getClass.getSimpleName} can't handle ${x}")
        }
      }
    }
    new PFC ()
  }

  def instructionAt (address: Int): Instruction[_] = {
    val data = programMemory.getData (address, 4)
    val instructionOpt = instructionSet (data)
    instructionOpt match {
      case None => throw new UnsupportedOperationException (s"${getClass.getSimpleName} could not parse instruction at ${toHex (ip, 6)} from ${data.map {toHex (_, 2)}.mkString (" ")}")
      case Some (i) => i
    }
  }

  def showVoltageAtPin (chipPin: String, voltage: Option[Double]) = {
    pinSamplers.get (chipPin) match {
      case Some (ps) => ps.addSample (engine.currentTick, voltage)
      case None =>
    }
  }

  def pinSampler (chipPin: String): PinSampler = {
    if (pinSamplers.contains (chipPin)) {
      pinSamplers (chipPin)
    }
    else {
      pinSamplers = pinSamplers + (chipPin -> new PinSampler (config.clockSpeed))
      pinSampler (chipPin)
    }
  }

  var logInstruction: Option[ExecutionLog => Unit] = None

  protected def handleCpuChange [C <: Cpu] (change: CpuChange[C]): Unit = {
    change match {
      case c: IncrementIp => ip += c.increment
      case c: SetIp => ip = c.newIp
      case c: ScheduleNextInstruction => handleScheduleNextInstruction ()
      case x => throw new UnsupportedOperationException (s"${getClass.getSimpleName} can't handle ${x}")
    }
  }

  private def handleInstruction [C <: Cpu] (instruction: Instruction[C]): Unit = {
    val events = instruction.execute (this.asInstanceOf[C])
    val tick = engine.currentTick + instruction.latency
    scheduleInstructionResults (instruction, tick, events)
    engine.schedule (ScheduleNextInstruction (), tick)
  }

  protected def scheduleInstructionResults (instruction: Instruction[_], tick: Long, events: Seq[Event]): Unit = {
    val tick = engine.currentTick + instruction.latency
    if (logInstruction.isDefined) {
      val comment = events.flatMap {
        case e: CpuChange[Cpu] => Some (e.mods (this))
        case e => Some (s"${e.getClass.getSimpleName}")
      }.mkString ("; ")
      logInstruction.get (ExecutionLog (engine.currentTick, ip, instruction.toString, comment))
    }
    events.foreach {engine.schedule (_, tick)}
  }

  private def handleScheduleNextInstruction (): Unit = {
    val instruction = instructionAt (_ip)
    engine.schedule (instruction, engine.currentTick)
  }

  // for testing only
  def setIpForTest (value: Int): Unit = {_ip = value}
}
