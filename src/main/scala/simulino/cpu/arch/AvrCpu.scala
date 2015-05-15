package simulino.cpu.arch

import java.util

import simulino.cpu.arch.avr.ATmega.{SetRegister, SetFlags, Flag}
import simulino.cpu.arch.avr.ATmega.Flag._
import simulino.cpu.arch.avr.AvrInstructionSet
import simulino.cpu.{CpuChange, Cpu}
import simulino.engine.Engine
import simulino.memory.UnsignedByte
import simulino.simulator.CpuConfiguration

/**
 * Created by dnwiebe on 5/13/15.
 */
class AvrCpu (val engine: Engine, val config: CpuConfiguration) extends Cpu {
  private val flagRegister = new FlagRegister ()
  private val registerFile = new RegisterFile ()
  private val _instructionSet = new AvrInstructionSet ()

  def instructionSet = _instructionSet
  def flag (name: Flag): Boolean = {flagRegister (name)}
  def setFlag (name: Flag, value: Boolean): Unit = {flagRegister (name) = value}
  def register (idx: Int): UnsignedByte = registerFile (idx)
  def setRegister (idx: Int, value: UnsignedByte): Unit = {registerFile (idx) = value}

  override def handleCpuChange (change: CpuChange): Unit = {
    change match {
      case c: SetRegister => setRegister (c.register, c.value)
      case c: SetFlags => handleSetFlags (c)
      case x => super.handleCpuChange (x)
    }
  }

  private def handleSetFlags (c: SetFlags): Unit = {
    List ((I, c.I), (T, c.T), (H, c.H), (S, c.S), (V, c.V), (N, c.N), (Z, c.Z), (C, c.C)).foreach {pair =>
      val (name, value) = pair
      value match {
        case None =>
        case Some (v) => setFlag (name, v)
      }
    }
  }
}

class RegisterFile () {
  private val registers = new Array[UnsignedByte] (32)
  (0 until registers.length).foreach {registers(_) = UnsignedByte (0)}

  def apply (idx: Int): UnsignedByte = registers(idx)
  def update (idx: Int, value: UnsignedByte) {registers(idx) = value}
}

class FlagRegister () {
  private val flags = new util.EnumMap[Flag, Boolean] (classOf[Flag])
  Flag.values.foreach {name => flags.put (name, false)}

  def apply (name: Flag): Boolean = {flags.get (name)}
  def update (name: Flag, value: Boolean) {flags.put (name, value)}
}