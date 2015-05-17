package simulino.cpu.arch.avr.ATmega

import simulino.cpu.arch.AvrCpu
import simulino.cpu.{IncrementIp, Instruction, InstructionObject}
import simulino.memory.UnsignedByte
import simulino.cpu.Implicits.RegisterBit
import simulino.cpu.arch.avr.ATmega.Flag._
import simulino.utils.Utils._

/**
  * Created by dnwiebe on 5/12/15.
  */

object ADD extends InstructionObject[ADD] {
  override val mask = 0xFC000000
  override val pattern = 0x0C000000
  override protected def parse (buffer: Array[UnsignedByte]): ADD = {
    new ADD (parseUnsignedParameter (buffer, 0x01F00000), parseUnsignedParameter (buffer, 0x020F0000))
  }
}

class ADD (val d: Int, val r: Int) extends Instruction[AvrCpu] {
  override def length = 2
  override def latency = 1
  override def execute (cpu: AvrCpu) = {
    val Rd = cpu.register (d)
    val Rr = cpu.register (r)
    val R = Rd + Rr
    val Hf = ((Rd bit 3) && (Rr bit 3)) || ((Rr bit 3) && !(R bit 3)) || (!(R bit 3) && (Rd bit 3))
    val Vf = ((Rd bit 7) && (Rr bit 7) && !(R bit 7)) || (!(Rd bit 7) && !(Rr bit 7) && (R bit 7))
    val Nf = R bit 7
    val Sf = Nf ^^ Vf
    val Zf = R == UnsignedByte (0)
    val Cf = ((Rd bit 7) && (Rr bit 7)) || ((Rr bit 7) && !(R bit 7)) || (!(R bit 7) && (Rd bit 7))
    List (IncrementIp (2), SetRegister (d, R), SetFlags (H = Some (Hf), V = Some (Vf), N = Some (Nf),
      S = Some (Sf), Z = Some(Zf), C = Some (Cf)))
  }
}

object CPC extends InstructionObject[CPC] {
  override val mask = 0xFC000000
  override val pattern = 0x04000000
  override protected def parse (buffer: Array[UnsignedByte]): CPC = {
    val d = parseUnsignedParameter (buffer, 0x01F00000)
    val r = parseUnsignedParameter (buffer, 0x020F0000)
    new CPC (d, r)
  }
}

class CPC (val d: Int, val r: Int) extends Instruction[AvrCpu] {
  override def length = 2
  override def latency = 1
  override def execute (cpu: AvrCpu) = {
    val Rd = cpu.register (d)
    val Rr = cpu.register (r)
    val Cp = if (cpu.flag (Flag.C)) 1 else 0
    val R = Rd - Rr - Cp
    val Hf = (!(Rd bit 3) && (Rr bit 3)) || ((Rr bit 3) && (R bit 3)) || ((R bit 3) && !(Rd bit 3))
    val Vf = ((R bit 7) && !(Rr bit 7) && !(R bit 7)) || (!(Rd bit 7) && (Rr bit 7) && (R bit 7))
    val Nf = R bit 7
    val Sf = Nf ^^ Vf
    val Zfopt = if (R == 0) None else Some (false)
    val Cf = (!(Rd bit 7) && (Rr bit 7)) || ((Rr bit 7) && (R bit 7)) || ((R bit 7) && !(Rd bit 7))
    List (IncrementIp (2), SetFlags (H = Some (Hf), V = Some (Vf), N = Some (Nf), S = Some (Sf),
      Z = Zfopt, C = Some (Cf)))
  }
}

object NOP extends InstructionObject[NOP] {
  override val mask = 0xFFFF0000
  override val pattern = 0x00000000
  override protected def parse (buffer: Array[UnsignedByte]): NOP = {
    new NOP ()
  }
}

class NOP () extends Instruction[AvrCpu] {
  override def length = 2
  override def latency = 1
  override def execute (cpu: AvrCpu) = List (IncrementIp (2))
}

object RJMP extends InstructionObject[RJMP] {
  override val mask = 0xF0000000
  override val pattern = 0xC0000000
  override protected def parse (buffer: Array[UnsignedByte]): RJMP = {
    val unsignedJump = parseUnsignedParameter (buffer, 0x0FFF0000)
    val signedJump = if ((unsignedJump & 0x800) == 0) unsignedJump else unsignedJump | 0xFFFFF000
    new RJMP (signedJump)
  }
}

class RJMP (val k: Int) extends Instruction[AvrCpu] {
  override def length = 2
  override def latency = 2
  override def execute (cpu: AvrCpu) = List (IncrementIp ((k + 1) * 2))
}

object SBC extends InstructionObject[SBC] {
  override val mask = 0xFC000000
  override val pattern = 0x08000000
  override protected def parse (buffer: Array[UnsignedByte]): SBC = {
    new SBC (parseUnsignedParameter (buffer, 0x01F00000), parseUnsignedParameter (buffer, 0x020F0000))
  }
}

class SBC (val d: Int, val r: Int) extends Instruction[AvrCpu] {
  override def length = 2
  override def latency = 1
  override def execute (cpu: AvrCpu) = {
    val Rd = cpu.register (d)
    val Rr = cpu.register (r)
    val R: UnsignedByte = Rd - Rr - (if (cpu.flag (C)) 1 else 0)
    val Hf = (!(Rd bit 3) && (Rr bit 3)) || ((Rr bit 3) && (R bit 3)) || ((R bit 3) && !(Rd bit 3))
    val Vf = ((Rd bit 7) && !(Rr bit 7) && !(R bit 7)) || (!(Rd bit 7) && (Rr bit 7) && (R bit 7))
    val Nf = R bit 7
    val Sf = Nf ^^ Vf
    val Zfopt = if (R == UnsignedByte (0)) None else Some (false)
    val Cf = (!(Rd bit 7) && (Rr bit 7)) || ((Rr bit 7) && (R bit 7)) || ((R bit 7) && !(Rd bit 7))
    List (IncrementIp (2), SetRegister (d, R), SetFlags (H = Some (Hf), V = Some (Vf), N = Some (Nf),
      S = Some (Sf), Z = Zfopt, C = Some (Cf)))
  }
}
