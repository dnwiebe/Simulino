package simulino.cpu

import simulino.engine.{Event, Engine, Subscriber}
import simulino.memory.Memory
import simulino.simulator.CpuConfiguration
import simulino.utils.Utils._

/**
 * Created by dnwiebe on 5/11/15.
 */
trait Cpu extends Subscriber {
  val engine: Engine
  val config: CpuConfiguration
  val programMemory: Memory
  val instructionSet: InstructionSet[_]

  private var _ip = 0
  private var _sp = 0

  def ip = _ip
  protected def ip_= (ip: Int): Unit = {
    _ip = ip
    engine.schedule (nextInstruction (), engine.nextTick)
  }

  def sp = _sp
  protected def sp_= (sp: Int): Unit = {_sp = sp}

  override def receive: PartialFunction[Event, Unit] = {
    class PFC[C <: Cpu] extends PartialFunction[Event, Unit] {
      override def isDefinedAt (event: Event): Boolean = {
        (event.getClass == classOf[CpuChange]) || (event.getClass == classOf[Instruction[_]])
      }
      override def apply (event: Event): Unit = {
        event match {
          case c: CpuChange => handleCpuChange (c)
          case i: Instruction[C] => handleInstruction (i.asInstanceOf[Instruction[Cpu]])
          case x => throw new UnsupportedOperationException (s"Cpu can't handle ${x}")
        }
      }
    }
    new PFC ()
  }

  def nextInstruction (): Instruction[_] = {
    val data = programMemory.getData (ip, 4)
    val instructionOpt = instructionSet (data)
    instructionOpt match {
      case None => throw new UnsupportedOperationException (s"${getClass} could not parse instruction from ${data.map {toHex (_, 2)}.mkString (" ")}")
      case Some (i) => i
    }
  }

  private def handleInstruction [C <: Cpu] (instruction: Instruction[C]): Unit = {
    val tick = engine.currentTick + instruction.latency
    instruction.execute (this.asInstanceOf[C]).foreach {event =>
      engine.schedule (event, tick)
    }
  }

  protected def handleCpuChange (change: CpuChange): Unit = {
    change match {
      case c: IncrementIp => ip += c.increment
      case c: SetIp => ip = c.newIp
      case c: IncrementSp => sp += c.increment
      case c: SetSp => sp = c.newSp
      case x => throw new UnsupportedOperationException (s"Cpu can't handle ${x}")
    }
  }
}
