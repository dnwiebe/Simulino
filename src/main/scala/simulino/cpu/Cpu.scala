package simulino.cpu

import simulino.engine.{Event, Engine, Subscriber}
import simulino.simulator.CpuConfiguration
import simulino.utils.Utils._

/**
 * Created by dnwiebe on 5/11/15.
 */
trait Cpu extends Subscriber {
  val engine: Engine
  val config: CpuConfiguration
  def instructionSet: InstructionSet[_]

  private var _ip = 0
  private var _sp = 0

  def ip = _ip
  protected def ip_= (ip: Int): Unit = {
    TEST_DRIVE_ME
  }

  def sp = _sp
  protected def sp_= (sp: Int): Unit = {
    TEST_DRIVE_ME
  }

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

  private def handleInstruction [C <: Cpu] (instruction: Instruction[C]): Unit = {
    val tick = engine.nextTick + instruction.latency
    instruction.execute (this.asInstanceOf[C]).foreach {event =>
      engine.schedule (event, tick)
    }
  }

  protected def handleCpuChange (change: CpuChange): Unit = {
    change match {
      case c: IncrementIp => _ip += c.increment
      case c: SetIp => _ip = c.newIp
      case c: IncrementSp => _sp += c.increment
      case c: SetSp => _sp = c.newSp
      case x => throw new UnsupportedOperationException (s"Cpu can't handle ${x}")
    }
  }
}
