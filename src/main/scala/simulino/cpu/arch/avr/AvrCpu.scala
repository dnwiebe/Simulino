package simulino.cpu.arch.avr

import simulino.cpu.arch.avr.ATmega.{Flag, SetFlags, SetRegister}
import simulino.cpu.{PushIp, SetMemory, Cpu, CpuChange}
import simulino.engine.Engine
import simulino.memory.{Span, Memory, UnsignedByte}
import simulino.simulator.CpuConfiguration
import simulino.utils.Utils._

/**
 * Created by dnwiebe on 5/13/15.
 */

object RegisterNames {
  val SPH = 0x5E
  val SPL = 0x5D
  val XL = 0x1A
  val XH = 0x1B
  val RAMPX = 0x59
  val SREG = 0x5F
}

class AvrCpu (val engine: Engine, val programMemory: Memory, val config: CpuConfiguration) extends Cpu {
  import RegisterNames._

  val registerFile = new RegisterFile ()
  val instructionSet = new AvrInstructionSet ()

  def register (idx: Int): UnsignedByte = registerFile (idx)
  def setRegister (idx: Int, value: UnsignedByte): Unit = {registerFile (idx) = value}

  override def sp: Int = ((register (SPH).value << 8) | register (SPL).value) & 0xFFFF
  protected override def sp_= (value: Int): Unit = {
    setRegister (SPH, (value >> 8) & 0xFF)
    setRegister (SPL, value & 0xFF)
  }

  override def handleCpuChange (change: CpuChange): Unit = {
    change match {
      case c: SetRegister => setRegister (c.register, c.value)
      case c: SetFlags => handleSetFlags (c)
      case c: WriteIOSpace => handleWriteIOSpace (c)
      case c: SetMemory => handleSetMemory (c)
      case c: PushIp => handlePushIp ()
      case x => super.handleCpuChange (x)
    }
  }

  def flag (name: Flag): Boolean = {
    val octet = register (SREG).value
    val idx = 7 - name.ordinal ()
    val shifted = octet >> idx
    (shifted & 0x01) == 1
  }

  private def handleSetFlags (c: SetFlags): Unit = {
    val original = register (SREG).value
    val withSets = original | (c.mask & c.pattern)
    val withSetsAndClears = withSets & (~c.mask | c.pattern)
    setRegister (SREG, withSetsAndClears)
  }

  private def handleWriteIOSpace (change: WriteIOSpace): Unit = {
    val address = change.address + 0x20
    setRegister (address, UnsignedByte (change.value))
  }

  private def handleSetMemory (change: SetMemory): Unit = {
    programMemory.update (change.address, change.value)
  }

  private def handlePushIp (): Unit = {
    val nextIp = ip + 2
    val firstByte = (nextIp >> 16) & 0xFF
    val secondByte = (nextIp >> 8) & 0xFF
    val thirdByte = nextIp & 0xFF
    programMemory.addSpan (Span (sp, Array(thirdByte, secondByte, firstByte)))
    sp = sp - 3
  }
}