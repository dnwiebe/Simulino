package simulino.cpu.arch.avr.ATmega

import simulino.cpu.arch.AvrCpu
import simulino.cpu.{SetIp, IncrementIp, Instruction, InstructionObject}
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
  override def toString = s"ADD R${d}, R${r}"
}

object CP extends InstructionObject[CP] {
  override val mask = 0xFC000000
  override val pattern = 0x14000000
  override protected def parse (buffer: Array[UnsignedByte]): CP = {
    val d = parseUnsignedParameter (buffer, 0x01F00000)
    val r = parseUnsignedParameter (buffer, 0x020F0000)
    new CP (d, r)
  }
}

class CP (val d: Int, val r: Int) extends Instruction[AvrCpu] {
  override def length = 2
  override def latency = 1
  override def execute (cpu: AvrCpu) = {
    val Rd = cpu.register (d)
    val Rr = cpu.register (r)
    val R = Rd - Rr
    val Hf = (!(Rd bit 3) && (Rr bit 3)) || ((Rr bit 3) && (R bit 3)) || ((R bit 3) && !(Rd bit 3))
    val Vf = ((R bit 7) && !(Rr bit 7) && !(R bit 7)) || (!(Rd bit 7) && (Rr bit 7) && (R bit 7))
    val Nf = R bit 7
    val Sf = Nf ^^ Vf
    val Zf = (R == 0)
    val Cf = (!(Rd bit 7) && (Rr bit 7)) || ((Rr bit 7) && (R bit 7)) || ((R bit 7) && !(Rd bit 7))
    List (IncrementIp (2), SetFlags (H = Some (Hf), V = Some (Vf), N = Some (Nf), S = Some (Sf),
      Z = Some (Zf), C = Some (Cf)))
  }
  override def toString = s"CP R${d}, R${r}"
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
  override def toString = s"CPC R${d}, R${r}"
}

object CPSE extends InstructionObject[CPSE] {
  override val mask = 0xFC000000
  override val pattern = 0x10000000
  override protected def parse (buffer: Array[UnsignedByte]): CPSE = {
    val d = parseUnsignedParameter (buffer, 0x01F00000)
    val r = parseUnsignedParameter (buffer, 0x020F0000)
    new CPSE (d, r)
  }
}

class CPSE (val d: Int, val r: Int) extends Instruction[AvrCpu] {
  private var latencyOpt: Option[Int] = None
  override def length = 2
  override def latency = latencyOpt.get
  override def execute (cpu: AvrCpu) = {
    val Rd = cpu.register (d)
    val Rr = cpu.register (r)
    if (Rd != Rr) {
      List (IncrementIp (handleEquality ()))
    }
    else {
      List (IncrementIp (handleInequality (cpu)))
    }
  }
  override def toString = s"CPSE R${d}, R${r}"

  private def handleInequality (cpu: AvrCpu): Int = {
    val nextLength = getNextInstructionLength (cpu)
    if (nextLength == 2) {
      handleTwoByteNextInstruction()
    }
    else {
      handleFourByteNextInstruction()
    }
  }

  private def handleEquality (): Int = {
    latencyOpt = Some (1)
    2
  }

  private def handleTwoByteNextInstruction (): Int = {
    latencyOpt = Some (2)
    4
  }

  private def handleFourByteNextInstruction (): Int = {
    latencyOpt = Some (3)
    6
  }

  private def getNextInstructionLength (cpu: AvrCpu): Int = {
    val buffer = cpu.programMemory.getData (cpu.ip + length, 2)
    val flags = List (
      ((buffer (0).value & 0x0E) == 0x0C) && ((buffer (1).value & 0xFE) == 0x94), // JMP
      ((buffer (0).value & 0x0F) == 0x00) && ((buffer (1).value & 0xFE) == 0x90), // LDS
      ((buffer (0).value & 0x0F) == 0x00) && ((buffer (1).value & 0xFE) == 0x92)  // STS
    )
    flags.find (f => f) match {
      case Some (_) => 4
      case None => 2
    }
  }
}

object EOR extends InstructionObject[EOR] {
  override val mask = 0xFC000000
  override val pattern = 0x24000000
  override protected def parse (buffer: Array[UnsignedByte]): EOR = {
    new EOR (parseUnsignedParameter (buffer, 0x01F00000), parseUnsignedParameter (buffer, 0x020F0000))
  }
}

class EOR (val d: Int, val r: Int) extends Instruction[AvrCpu] {
  override def length = 2
  override def latency = 1
  override def execute (cpu: AvrCpu) = {
    val Rd = cpu.register (d)
    val Rr = cpu.register (r)
    val R = Rd ^ Rr
    val Vf = false
    val Nf = (R bit 7)
    val Sf = Vf ^^ Nf
    val Zf = (R == 0)
    List (IncrementIp (2), SetRegister (d, R), SetFlags (S = Some (Sf), V = Some (Vf), N = Some (Nf), Z = Some (Zf)))
  }
  override def toString = s"EOR R${d}, R${r}"
}

// Not available in all CPUs; here temporarily so that CPSE has a four-byte instruction to skip
object JMP extends InstructionObject[JMP] {
  override val mask = 0xFE0E0000
  override val pattern = 0x940C0000
  override protected def parse (buffer: Array[UnsignedByte]): JMP = {
    val k = parseUnsignedParameter (buffer, 0x01F1FFFF)
    new JMP (k)
  }
}

class JMP (k: Int) extends Instruction[AvrCpu] {
  override def length = 4
  override def latency = 3
  override def execute (cpu: AvrCpu) = List (SetIp (k << 1))
  override def toString = s"JMP ${k}"
}

object MULS extends InstructionObject[MULS] {
  override val mask = 0xFF000000
  override val pattern = 0x02000000
  override protected def parse (buffer: Array[UnsignedByte]): MULS = {
    val d = parseUnsignedParameter (buffer, 0x00F00000)
    val r = parseUnsignedParameter (buffer, 0x000F0000)
    new MULS (d, r)
  }
}

class MULS (d: Int, r: Int) extends Instruction[AvrCpu] {
  override def length = 2
  override def latency = 2
  override def execute (cpu: AvrCpu) = {
    val Rd: Int = cpu.register (d)
    val Rr: Int = cpu.register (r)
    val R = Rd * Rr
    val Cf = ((R & 0x8000) != 0)
    val Zf = (R == 0)
    List (IncrementIp (2), SetRegister (1, ((R >> 8) & 0xFF)), SetRegister (0, (R & 0xFF)),
      SetFlags (C = Some (Cf), Z = Some (Zf)))
  }
  override def toString = s"MULS R${d}, R${r}"
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
  override def toString = s"NOP"
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
  override def toString = s"RJMP ${k}"
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
  override def toString = s"SBC R${d}, R${r}"
}
