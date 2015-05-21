package simulino.cpu.arch.avr

import simulino.cpu.arch.avr.ATmega.{Flag, SetFlags, SetRegister}
import simulino.cpu.{SetMemory, Cpu, CpuChange}
import simulino.engine.Engine
import simulino.memory.{Memory, UnsignedByte}
import simulino.simulator.CpuConfiguration
import simulino.utils.Utils._

/**
 * Created by dnwiebe on 5/13/15.
 */

object RegisterNames {
  val XL = 0x1A
  val XH = 0x1B
  val RAMPX = 0x59
  val SREG = 0x5F
}

class AvrCpu (val engine: Engine, val programMemory: Memory, val config: CpuConfiguration) extends Cpu {
  val registerFile = new RegisterFile ()
  val instructionSet = new AvrInstructionSet ()

  def register (idx: Int): UnsignedByte = registerFile (idx)
  def setRegister (idx: Int, value: UnsignedByte): Unit = {registerFile (idx) = value}

  override def sp: Int = ((register (0x5E).value << 8) | register (0x5D).value) & 0xFFFF
  protected override def sp_= (sp: Int): Unit = {TEST_DRIVE_ME}

  override def handleCpuChange (change: CpuChange): Unit = {
    change match {
      case c: SetRegister => setRegister (c.register, c.value)
      case c: SetFlags => handleSetFlags (c)
      case c: WriteIOSpace => handleWriteIOSpace (c)
      case c: SetMemory => handleSetMemory (c)
      case x => super.handleCpuChange (x)
    }
  }

  def flag (name: Flag): Boolean = {
    val octet = register (0x5F).value
    val idx = 7 - name.ordinal ()
    val shifted = octet >> idx
    (shifted & 0x01) == 1
  }

  private def handleSetFlags (c: SetFlags): Unit = {
    val flags = List (c.I, c.T, c.H, c.S, c.V, c.N, c.Z, c.C)
    val (andMask, orMask) = flags.foldLeft ((0, 0)) {(soFar, bitValue) =>
      val (andMask, orMask) = soFar
      val andShifted = andMask << 1
      val orShifted = orMask << 1
      bitValue match {
        case None => (andShifted | 1, orShifted)
        case Some (true) => (andShifted | 1, orShifted | 1)
        case Some (false) => (andShifted, orShifted)
      }
    }
    val octet = register (0x5F).value & andMask | orMask
    setRegister (0x5F, octet)
  }

  private def handleWriteIOSpace (change: WriteIOSpace): Unit = {
    val address = change.address + 0x20
    setRegister (address, UnsignedByte (change.value))
  }

  private def handleSetMemory (change: SetMemory): Unit = {
    programMemory.update (change.address, change.value)
  }
}
