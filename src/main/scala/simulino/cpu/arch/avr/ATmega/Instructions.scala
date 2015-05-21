package simulino.cpu.arch.avr.ATmega

import simulino.cpu.arch.avr.{AvrCpu, WriteIOSpace}
import simulino.cpu._
import simulino.memory.UnsignedByte
import simulino.cpu.Implicits.RegisterBit
import simulino.cpu.arch.avr.ATmega.Flag._
import simulino.cpu.arch.avr.RegisterNames._
import simulino.utils.Utils._

/**
  * Created by dnwiebe on 5/12/15.
  */

object ADD extends AvrInstructionObject[ADD] {
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

object BRBC extends AvrInstructionObject[BRBC] {
  override val mask = 0xFC000000
  override val pattern = 0xF4000000
  override protected def parse (buffer: Array[UnsignedByte]): BRBC = {
    val s = parseUnsignedParameter (buffer, 0x00070000)
    val unsignedK = parseUnsignedParameter (buffer, 0x03F80000)
    val k = if ((unsignedK & 0x40) == 0) unsignedK else (unsignedK | 0xFFFFFF80)
    new BRBC (s, k)
  }
}

class BRBC (val s: Int, val k: Int) extends Instruction[AvrCpu] {
  private var latencyOpt: Option[Int] = None
  override def length = 2
  override def latency = latencyOpt.get
  override def execute (cpu: AvrCpu) = {
    val sreg = cpu.register (SREG).value
    if ((sreg & (1 << s)) == 0) {
      latencyOpt = Some (2)
      List (IncrementIp ((k + 1) * 2))
    }
    else {
      latencyOpt = Some (1)
      List (IncrementIp (2))
    }
  }
  override def toString = s"BRBC ${s}, ${k}"
}

object CP extends AvrInstructionObject[CP] {
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

object CPC extends AvrInstructionObject[CPC] {
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

object CPI extends AvrInstructionObject[CPI] {
  override val mask = 0xF0000000
  override val pattern = 0x30000000
  override protected def parse (buffer: Array[UnsignedByte]): CPI = {
    val d = parseUnsignedParameter (buffer, 0x00F00000)
    val K = parseUnsignedParameter (buffer, 0x0F0F0000)
    new CPI (d + 0x10, K)
  }
}

class CPI (val d: Int, val K: UnsignedByte) extends Instruction[AvrCpu] {
  override def length = 2
  override def latency = 1
  override def execute (cpu: AvrCpu) = {
    val Rd = cpu.register (d)
    val R = Rd - K
    val Hf = (!(Rd bit 3) && (K bit 3)) || ((K bit 3) && (R bit 3)) || ((R bit 3) && !(Rd bit 3))
    val Vf = ((Rd bit 7) && !(K bit 7) && !(R bit 7)) || (!(Rd bit 7) && (K bit 7) && (R bit 7))
    val Nf = (R bit 7)
    val Sf = Nf ^^ Vf
    val Zf = R.value == 0
    val Cf = (!(Rd bit 7) && (K bit 7)) || ((K bit 7) && (R bit 7)) || ((R bit 7) && !(Rd bit 7))
    List (IncrementIp (2), SetFlags (H = Some (Hf), S = Some (Sf), V = Some (Vf), N = Some (Nf), Z = Some (Zf), C = Some (Cf)))
  }
  override def toString = s"CPI R${d}, ${K.value}"
}

object CPSE extends AvrInstructionObject[CPSE] {
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

object EOR extends AvrInstructionObject[EOR] {
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
object JMP extends AvrInstructionObject[JMP] {
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

object LDI extends AvrInstructionObject[LDI] {
  override val mask = 0xF0000000
  override val pattern = 0xE0000000
  override protected def parse (buffer: Array[UnsignedByte]): LDI = {
    val d = parseUnsignedParameter (buffer, 0x00F00000)
    val K = parseUnsignedParameter (buffer, 0x0F0F0000)
    new LDI (0x10 + d, K)
  }
}

class LDI (val d: Int, val K: Int) extends Instruction[AvrCpu] {
  override def length = 2
  override def latency = 1
  override def execute (cpu: AvrCpu) = List (IncrementIp (2), SetRegister (d, K))
  override def toString = s"LDI R${d}, ${K}"
}

object MULS extends AvrInstructionObject[MULS] {
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

object NOP extends AvrInstructionObject[NOP] {
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

object OUT extends AvrInstructionObject[OUT] {
  override val mask = 0xF8000000
  override val pattern = 0xB8000000
  override protected def parse (buffer: Array[UnsignedByte]): OUT = {
    val A = parseUnsignedParameter (buffer, 0x060F0000)
    val r = parseUnsignedParameter (buffer, 0x01F00000)
    new OUT (A, r)
  }
}

class OUT (val A: Int, val r: Int) extends Instruction[AvrCpu] {
  override def length = 2
  override def latency = 1
  override def execute (cpu: AvrCpu) = {
    val Rr = cpu.register (r)
    List (IncrementIp (2), WriteIOSpace (A, Rr.value))
  }
  override def toString = s"OUT $$${toHex (A, 2)}, R${r}"
}

object RJMP extends AvrInstructionObject[RJMP] {
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

object SBC extends AvrInstructionObject[SBC] {
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

object ST extends AvrInstructionObject[ST] {
  override val mask = 0xFE080000
  override val pattern = 0x92080000

  override def apply (buffer: Array[UnsignedByte]): Option[ST] = {
    super.apply (buffer) match {
      case None => None
      case Some (instruction) => if ((buffer(0).value & 0x0F) == 0x0F) None else Some (instruction)
    }
  }

  override protected def parse (buffer: Array[UnsignedByte]): ST = {
    val idx = parseUnsignedParameter (buffer, 0x00030000)
    val x = IndirectionType.fromIndex (idx)
    val r = parseUnsignedParameter (buffer, 0x01F00000)
    new ST (x, r)
  }
}

class ST (val x: IndirectionType, val r: Int) extends Instruction[AvrCpu] {
  override def length = 2
  override def latency = 2
  override def execute (cpu: AvrCpu) = {
    val Rr = cpu.register (r)
    val address = (cpu.register (RAMPX).value << 16) | (cpu.register (XH).value << 8) | cpu.register (XL).value
    val preAddress = x.preOperate (address)
    val setMemory = SetMemory (preAddress, Rr)
    val postAddress = x.postOperate (preAddress)
    val setRAMPX = SetRegister (RAMPX, (postAddress >> 16) & 0xFF)
    val setXH = SetRegister (XH, (postAddress >> 8) & 0xFF)
    val setXL = SetRegister (XL, postAddress & 0xFF)
    List (IncrementIp (2), setRAMPX, setXH, setXL, setMemory)
  }
  override def toString = s"ST ${x.toString ("X")}, R${r}"
}
