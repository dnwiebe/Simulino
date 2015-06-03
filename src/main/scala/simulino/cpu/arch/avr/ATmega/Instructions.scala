package simulino.cpu.arch.avr.ATmega

import simulino.cpu.arch.avr.AvrCpu
import simulino.cpu._
import simulino.memory.UnsignedByte
import simulino.cpu.Implicits.RegisterBit
import simulino.cpu.arch.avr.ATmega.Flag._
import simulino.cpu.arch.avr.RegisterNames._
import simulino.utils.Utils._

/**
  * Created by dnwiebe on 5/12/15.
  */

trait ADxObj[T <: Instruction[AvrCpu]] extends AvrInstructionObject[T] {
  override val mask = 0xFC000000
  override protected def parse (buffer: Array[UnsignedByte]): T = {
    val d = parseUnsignedParameter (buffer, 0x01F00000)
    val r = parseUnsignedParameter (buffer, 0x020F0000)
    make (d, r)
  }
  protected def make (d: Int, r: Int): T
}

trait ADxCls[T] extends Instruction[AvrCpu] {
  val d: Int
  val r: Int
  override def length = 2
  override def latency = 1
  override def execute (cpu: AvrCpu) = {
    val Rd = cpu.register (d)
    val Rr = cpu.register (r)
    val R = op (cpu, Rd, Rr)
    val Hf = ((Rd bit 3) && (Rr bit 3)) || ((Rr bit 3) && !(R bit 3)) || (!(R bit 3) && (Rd bit 3))
    val Vf = ((Rd bit 7) && (Rr bit 7) && !(R bit 7)) || (!(Rd bit 7) && !(Rr bit 7) && (R bit 7))
    val Nf = R bit 7
    val Sf = Nf ^^ Vf
    val Zf = R == UnsignedByte (0)
    val Cf = ((Rd bit 7) && (Rr bit 7)) || ((Rr bit 7) && !(R bit 7)) || (!(R bit 7) && (Rd bit 7))
    List (IncrementIp (2), SetMemory (d, R), SetFlags (H = Some (Hf), V = Some (Vf), N = Some (Nf),
      S = Some (Sf), Z = Some(Zf), C = Some (Cf)))
  }
  override def toString = s"${getClass.getSimpleName} R${d}, R${r}"
  protected def op (cpu: AvrCpu, Rd: UnsignedByte, Rr: UnsignedByte): UnsignedByte
}

object ADC extends ADxObj[ADC] {
  override val pattern = 0x1C000000
  override protected def make (d: Int, r: Int): ADC = new ADC (d, r)
}

class ADC (val d: Int, val r: Int) extends ADxCls[ADC] {
  override protected def op (cpu: AvrCpu, Rd: UnsignedByte, Rr: UnsignedByte) = Rd + Rr + (cpu.register(SREG).value & 0x01)
}

object ADD extends ADxObj[ADD] {
  override val pattern = 0x0C000000
  override protected def make (d: Int, r: Int): ADD = new ADD (d, r)
}

class ADD (val d: Int, val r: Int) extends ADxCls[ADD] {
  override protected def op (cpu: AvrCpu, Rd: UnsignedByte, Rr: UnsignedByte) = Rd + Rr
}

object ADIW extends AvrInstructionObject[ADIW] {
  override val mask = 0xFF000000
  override val pattern = 0x96000000
  override protected def parse (buffer: Array[UnsignedByte]): ADIW = {
    val preD = parseUnsignedParameter (buffer, 0x00300000)
    val d = (preD * 2) + 24
    val K = parseUnsignedParameter (buffer, 0x00CF0000)
    new ADIW (d, K)
  }
}

class ADIW (val d: Int, val K: Int) extends Instruction[AvrCpu] {
  override def length = 2
  override def latency = 2
  override def execute (cpu: AvrCpu) = {
    val Rd = ((cpu.register (d + 1).value & 0xFF) << 8) + (cpu.register (d).value & 0xFF)
    val Rdh = UnsignedByte (Rd >> 8)
    val R = Rd + K
    val Rh = UnsignedByte (R >> 8)
    val Rl = UnsignedByte (R & 0xFF)
    val Vf = !(Rdh bit 7) && (Rh bit 15)
    val Nf = (Rh bit 15)
    val Sf = Vf ^^ Nf
    val Zf = (R == 0)
    val Cf = !(Rh bit 7) && (Rdh bit 7)
    List (IncrementIp (2), SetMemory (d + 1, Rh), SetMemory (d, Rl), SetFlags (S = Some (Sf), V = Some (Vf),
      N = Some (Nf), Z = Some (Zf), C = Some (Cf)))
  }
  override def toString = {
    val lhs = d match {
      case 24 => "r25:24"
      case 26 => "XH:XL"
      case 28 => "YH:YL"
      case 30 => "ZH:ZL"
      case x => s"??${x}??"
    }
    s"ADIW ${lhs}, $$${toHex (K, 2)}"
  }
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

object CLx extends AvrInstructionObject[CLx] {
  override val mask = 0xFF8F0000
  override val pattern = 0x94880000
  override protected def parse (buffer: Array[UnsignedByte]): CLx = {
    parseUnsignedParameter (buffer, 0x00700000) match {
      case _ => TEST_DRIVE_ME
    }
  }
}

abstract class CLx (flagMask: Int) extends Instruction[AvrCpu] {
  override def length = {TEST_DRIVE_ME; 2}
  override def latency = {TEST_DRIVE_ME; 1}
  override def execute (cpu: AvrCpu) = {
    TEST_DRIVE_ME
    List (IncrementIp (2), SetFlags (1 << flagMask, 0xFF))
  }
  override def toString = s"CL${flagName}"
  private def flagName = getClass.getSimpleName.last
}

class CLI extends CLx (7)

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
    List (IncrementIp (2), SetMemory (d, R), SetFlags (S = Some (Sf), V = Some (Vf), N = Some (Nf), Z = Some (Zf)))
  }
  override def toString = s"EOR R${d}, R${r}"
}

object IN extends AvrInstructionObject[IN] {
  override val mask = 0xF8000000
  override val pattern = 0xB0000000
  override protected def parse (buffer: Array[UnsignedByte]): IN = {
    val d = parseUnsignedParameter (buffer, 0x01F00000)
    val A = parseUnsignedParameter (buffer, 0x060F0000)
    new IN (d, A)
  }
}

class IN (val d: Int, val A: Int) extends Instruction[AvrCpu] {
  override def length = 2
  override def latency = 1
  override def execute (cpu: AvrCpu) = {
    val R = cpu.register (A + 0x20)
    List (IncrementIp (2), SetMemory (d, R))
  }
  override def toString = s"IN R${d}, $$${toHex (A, 2)}"
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

object LDD extends ComplexAvrInstructionObject[LDD] {
  val maskPatternPairs = List (
    (0xD2080000, 0x80080000),
    (0xFE0F0000, 0x90090000),
    (0xFE0F0000, 0x900A0000),
    (0xD2080000, 0x80000000),
    (0xFE0F0000, 0x90010000),
    (0xFE0F0000, 0x90020000)
  )

  override protected def parse (buffer: Array[UnsignedByte]): LDD = {
    val d = parseUnsignedParameter (buffer, 0x01F00000)
    val r = (buffer(0).value & 0x08) match {
      case 0x08 => 'Y'
      case 0x00 => 'Z'
    }
    val (x, q) = parseUnsignedParameter (buffer, 0x10030000) match {
      case 0x5 => (IndirectionType.PostIncrement, 0)
      case 0x6 => (IndirectionType.PreDecrement, 0)
      case _ => (IndirectionType.Unchanged, parseUnsignedParameter (buffer, 0x2C070000))
    }
    new LDD (d, r, x, q)
  }
}

class LDD (val d: Int, val r: Char, val x: IndirectionType, val q: Int) extends Instruction[AvrCpu] {
  override def length = 2
  override def latency = {
    if (q > 0) 2
    else if (x == IndirectionType.PostIncrement) 2
    else if (x == IndirectionType.PreDecrement) 3
    else 1
  }
  override def execute (cpu: AvrCpu) = {
    val regTuple = if (r == 'Y') Yfull else Zfull
    val initialValue = getExtended (cpu, regTuple)
    val preValue = x match {
      case IndirectionType.Unchanged => initialValue + q
      case IndirectionType.PostIncrement => initialValue
      case IndirectionType.PreDecrement => initialValue - 1
    }
    val R = cpu.register (preValue)
    val postValue = x match {
      case IndirectionType.Unchanged => initialValue
      case IndirectionType.PostIncrement => preValue + 1
      case IndirectionType.PreDecrement => preValue
    }
    val regMod = if (postValue != initialValue) setExtended (regTuple, postValue) else Nil
    List (IncrementIp (2), SetMemory (d, R)) ++ regMod
  }
  override def toString = {
    x match {
      case IndirectionType.Unchanged => {
        q match {
          case 0 => s"LD R${d}, ${r}"
          case _ => s"LDD R${d}, ${r}+${q}"
        }
      }
      case IndirectionType.PostIncrement => s"LD R${d}, ${r}+"
      case IndirectionType.PreDecrement => s"LD R${d}, -${r}"
    }
  }
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
  override def execute (cpu: AvrCpu) = List (IncrementIp (2), SetMemory (d, K))
  override def toString = s"LDI R${d}, $$${toHex (K, 2)}"
}

object LDS extends AvrInstructionObject[LDS] {
  override val mask = 0xFE0F0000
  override val pattern = 0x90000000
  override protected def parse (buffer: Array[UnsignedByte]): LDS = {
    val d = parseUnsignedParameter (buffer, 0x01F00000)
    val k = parseUnsignedParameter (buffer, 0x0000FFFF)
    new LDS (d, k)
  }
}

class LDS (val d: Int, val k: Int) extends Instruction[AvrCpu] {
  override def length = 4
  override def latency = 2
  override def execute (cpu: AvrCpu) = {
    val K = cpu.register (k)
    List (IncrementIp (4), SetMemory (d, K))
  }
  override def toString = s"LDS R${d}, $$${toHex (k, 4)}"
}

object MOVW extends AvrInstructionObject[MOVW] {
  override val mask = 0xFF000000
  override val pattern = 0x01000000
  override protected def parse (buffer: Array[UnsignedByte]): MOVW = {
    val rawD = parseUnsignedParameter (buffer, 0x00F00000)
    val rawR = parseUnsignedParameter (buffer, 0x000F0000)
    new MOVW (rawD << 1, rawR << 1)
  }
}

class MOVW (val d: Int, val r: Int) extends Instruction[AvrCpu] {
  override def length = 2
  override def latency = 1
  override def execute (cpu: AvrCpu) = {
    val RrL = cpu.register (r)
    val RrH = cpu.register (r + 1)
    List (IncrementIp (2), SetMemory (d, RrL), SetMemory (d + 1, RrH))
  }
  override def toString = s"MOVW R${d + 1}:${d}, R${r + 1}:${r}"
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
    List (IncrementIp (2), SetMemory (1, ((R >> 8) & 0xFF)), SetMemory (0, (R & 0xFF)),
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

object ORI extends AvrInstructionObject[ORI] {
  override val mask = 0xF0000000
  override val pattern = 0x60000000
  override protected def parse (buffer: Array[UnsignedByte]): ORI = {
    val d = parseUnsignedParameter (buffer, 0x00F00000)
    val K = parseUnsignedParameter (buffer, 0x0F0F0000)
    new ORI (d + 0x10, K)
  }
}

class ORI (val d: Int, val K: Int) extends Instruction[AvrCpu] {
  override def length = 2
  override def latency = 1
  override def execute (cpu: AvrCpu) = {
    val Rd = cpu.register (d)
    val R = Rd | K
    val Nf = (R bit 7)
    val Sf = Nf
    val Zf = (R == 0)
    List (IncrementIp (2), SetMemory (d, R), SetFlags (S = Some (Sf), V = Some (false), N = Some (Nf), Z = Some (Zf)))
  }
  override def toString = s"ORI R${d}, $$${toHex (K, 2)}"
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
    List (IncrementIp (2), SetMemory (A + 0x20, Rr.value))
  }
  override def toString = s"OUT $$${toHex (A, 2)}, R${r}"
}

object POP extends AvrInstructionObject[POP] {
  override val mask = 0xFE0F0000
  override val pattern = 0x900F0000
  override protected def parse (buffer: Array[UnsignedByte]): POP = {
    val d = parseUnsignedParameter (buffer, 0x01F00000)
    new POP (d)
  }
}

class POP (val d: Int) extends Instruction[AvrCpu] {
  override def length = 2
  override def latency = 2
  override def execute (cpu: AvrCpu) = {
    List (IncrementIp (2), Pop (d))
  }
  override def toString = s"POP R${d}"
}

object PUSH extends AvrInstructionObject[PUSH] {
  override val mask = 0xFE0F0000
  override val pattern = 0x920F0000
  override protected def parse (buffer: Array[UnsignedByte]): PUSH = {
    val r = parseUnsignedParameter (buffer, 0x01F00000)
    new PUSH (r)
  }
}

class PUSH (val r: Int) extends Instruction[AvrCpu] {
  override def length = 2
  override def latency = 2
  override def execute (cpu: AvrCpu) = {
    val R = cpu.register (r)
    List (IncrementIp (2), Push (R))
  }
  override def toString = s"PUSH R${r}"
}

object RCALL extends AvrInstructionObject[RCALL] {
  override val mask = 0xF0000000
  override val pattern = 0xD0000000
  override protected def parse (buffer: Array[UnsignedByte]): RCALL = {
    val unsignedK = parseUnsignedParameter (buffer, 0x0FFF0000)
    val k = if ((unsignedK & 0x800) == 0) unsignedK else (unsignedK | 0xFFFFF000)
    new RCALL (k)
  }
}

class RCALL (val k: Int) extends Instruction[AvrCpu] {
  override def length = 2
  override def latency = 4
  override def execute (cpu: AvrCpu) = {
    List (PushIp (), IncrementIp ((k + 1) * 2))
  }
  override def toString = s"RCALL ${k}"
}

object RET extends AvrInstructionObject[RET] {
  override val mask = 0xFFFF0000
  override val pattern = 0x95080000
  override protected def parse (buffer: Array[UnsignedByte]): RET = {
    new RET ()
  }
}

class RET extends Instruction[AvrCpu] {
  override def length = 2
  override def latency = 5
  override def execute (cpu: AvrCpu) = {
    List (PopIp ())
  }
  override def toString = "RET"
}

object RETI extends AvrInstructionObject[RETI] {
  override val mask = 0xFFFF0000
  override val pattern = 0x95180000
  override protected def parse (buffer: Array[UnsignedByte]): RETI = {
    new RETI
  }
}

class RETI extends Instruction[AvrCpu] {
  override def length = 2
  override def latency = 5
  override def execute (cpu: AvrCpu) = {
    List (PopIp (), SetFlags (I = Some (true)))
  }
  override def toString = "RETI"
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
    List (IncrementIp (2), SetMemory (d, R), SetFlags (H = Some (Hf), V = Some (Vf), N = Some (Nf),
      S = Some (Sf), Z = Zfopt, C = Some (Cf)))
  }
  override def toString = s"SBC R${d}, R${r}"
}

object SEx extends AvrInstructionObject[SEx] {
  override val mask = 0xFF8F0000
  override val pattern = 0x94080000
  override protected def parse (buffer: Array[UnsignedByte]): SEx = {
    parseUnsignedParameter (buffer, 0x00700000) match {
      case 7 => new SEI ()
      case _ => TEST_DRIVE_ME
    }
  }
}

abstract class SEx (flagMask: Int) extends Instruction[AvrCpu] {
  override def length = 2
  override def latency = 1
  override def execute (cpu: AvrCpu) = {
    List (IncrementIp (2), SetFlags (1 << flagMask, 0xFF))
  }
  override def toString = s"SE${flagName}"
  private def flagName = getClass.getSimpleName.last
}

class SEI extends SEx (7)

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
    val address = getExtended (cpu, Xfull)
    val preAddress = x.preOperate (address)
    val setMemory = SetMemory (preAddress, Rr)
    val postAddress = x.postOperate (preAddress)
    List (IncrementIp (2), setMemory) ++ setExtended (Xfull, postAddress)
  }
  override def toString = s"ST ${x.toString ("X")}, R${r}"
}

object STD extends ComplexAvrInstructionObject[STD] {
  val maskPatternPairs = List (
    (0xD2080000, 0x82080000),
    (0xFE0F0000, 0x92090000),
    (0xFE0F0000, 0x920A0000),
    (0xD2080000, 0x82000000),
    (0xFE0F0000, 0x92010000),
    (0xFE0F0000, 0x92020000)
  )

  override protected def parse (buffer: Array[UnsignedByte]): STD = {
    val r = parseUnsignedParameter (buffer, 0x01F00000)
    val d = (buffer(0).value & 0x08) match {
      case 0x08 => 'Y'
      case 0x00 => 'Z'
    }
    val (x, q) = parseUnsignedParameter (buffer, 0x10030000) match {
      case 0x5 => (IndirectionType.PostIncrement, 0)
      case 0x6 => (IndirectionType.PreDecrement, 0)
      case _ => (IndirectionType.Unchanged, parseUnsignedParameter (buffer, 0x2C070000))
    }
    new STD (d, r, x, q)
  }
}

class STD (val d: Char, val r: Int, val x: IndirectionType, val q: Int) extends Instruction[AvrCpu] {
  override def length = 2
  override def latency = 2
  override def execute (cpu: AvrCpu) = {
    val regTuple = if (d == 'Y') Yfull else Zfull
    val initialValue = getExtended (cpu, regTuple)
    val preValue = x match {
      case IndirectionType.Unchanged => initialValue + q
      case IndirectionType.PostIncrement => initialValue
      case IndirectionType.PreDecrement => initialValue - 1
    }
    val R = cpu.register (r)
    val postValue = x match {
      case IndirectionType.Unchanged => initialValue
      case IndirectionType.PostIncrement => preValue + 1
      case IndirectionType.PreDecrement => preValue
    }
    val regMod = if (postValue != initialValue) setExtended (regTuple, postValue) else Nil
    List (IncrementIp (2), SetMemory (preValue, R)) ++ regMod
  }
  override def toString = {
    x match {
      case IndirectionType.Unchanged => {
        q match {
          case 0 => s"ST ${d}, R${r}"
          case _ => s"STD ${d}+${q}, R${r}"
        }
      }
      case IndirectionType.PostIncrement => s"ST ${d}+, R${r}"
      case IndirectionType.PreDecrement => s"ST -${d}, R${r}"
    }
  }
}

object STS extends AvrInstructionObject[STS] {
  override val mask = 0xFE0F0000
  override val pattern = 0x92000000
  override protected def parse (buffer: Array[UnsignedByte]): STS = {
    val k = parseUnsignedParameter (buffer, 0x0000FFFF)
    val r = parseUnsignedParameter (buffer, 0x01F00000)
    new STS (k, r)
  }
}

class STS (val k: Int, val r: Int) extends Instruction[AvrCpu] {
  override def length = 4
  override def latency = 2
  override def execute (cpu: AvrCpu) = {
    val Rr = cpu.register (r)
    List (IncrementIp (4), SetMemory (k, Rr))
  }
  override def toString = s"STS $$${toHex (k, 2)}, R${r}"
}

object SUBI extends AvrInstructionObject[SUBI] {
  override val mask = 0xF0000000
  override val pattern = 0x50000000
  override protected def parse (buffer: Array[UnsignedByte]): SUBI = {
    val rawD = parseUnsignedParameter (buffer, 0x00F00000)
    val K = parseUnsignedParameter (buffer, 0x0F0F0000)
    new SUBI (rawD + 0x10, K)
  }
}

class SUBI (val d: Int, val K: UnsignedByte) extends Instruction[AvrCpu] {
  override def length = 2
  override def latency = 1
  override def execute (cpu: AvrCpu) = {
    val Rd = cpu.register (d)
    val R = Rd - K
    val Hf = (!(Rd bit 3) && (K bit 3)) || ((K bit 3) && (R bit 3)) || ((R bit 3) && !(Rd bit 3))
    val Vf = ((Rd bit 7) && !(K bit 7) && !(R bit 7)) || (!(Rd bit 7) && (K bit 7) && (R bit 7))
    val Nf = R bit 7
    val Sf = Nf ^^ Vf
    val Zf = R.value == 0
    val Cf = (!(Rd bit 7) && (K bit 7)) || ((K bit 7) && (R bit 7)) || ((R bit 7) && !(Rd bit 7))
    List (IncrementIp (2), SetMemory (d, R), SetFlags (H = Some (Hf), S = Some (Sf), V = Some (Vf), N = Some (Nf),
      Z = Some (Zf), C = Some (Cf)))
  }
  override def toString = s"SUBI R${d}, $$${toHex (K, 2)}"
}
