package simulino.cpu.arch.avr.ATmega

import simulino.cpu.arch.avr._
import simulino.cpu._
import simulino.engine.Event
import simulino.memory.UnsignedByte
import simulino.cpu.Implicits.RegisterBit
import simulino.cpu.arch.avr.ATmega.Flag._
import simulino.cpu.arch.avr.ATmega.IndirectionType._
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

trait ADxCls[T] extends Instruction[AvrCpu] with AvrInstructionUtils {
  val d: Int
  val r: Int
  override def length = 2
  override def latency = 1
  override def execute (cpu: AvrCpu) = {
    val Rd = cpu.getMemory (d)
    val Rr = cpu.getMemory (r)
    val R = op (cpu, Rd, Rr)
    val setFlags = builder (R, Rd, Rr)
      .negative (Some (R bit 7))
      .zero (Some (R.value == 0))
      .make ()
    List (SetMemory (d, R), setFlags)
  }
  override def toString = s"${getClass.getSimpleName} R${d}, R${r}"
  protected def op (cpu: AvrCpu, Rd: UnsignedByte, Rr: UnsignedByte): UnsignedByte
}

object ADC extends ADxObj[ADC] {
  override val pattern = 0x1C000000
  override protected def make (d: Int, r: Int): ADC = new ADC (d, r)
}

class ADC (val d: Int, val r: Int) extends ADxCls[ADC] {
  override protected def op (cpu: AvrCpu, Rd: UnsignedByte, Rr: UnsignedByte) = Rd + Rr + (cpu.getMemory(SREG).value & 0x01)
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
    val Rdh = cpu.getMemory (d + 1)
    val Rdl = cpu.getMemory (d)
    val Rd = intFromBytes (Rdh, Rdl)
    val R = (Rd + K) & 0xFFFF
    val Rh = UnsignedByte (R >> 8)
    val Rl = UnsignedByte (R & 0xFF)
    val Vf = !(Rdh bit 7) && (Rh bit 15)
    val Nf = (Rh bit 15)
    val Sf = Vf ^^ Nf
    val Zf = (R == 0)
    val Cf = !(Rh bit 7) && (Rdh bit 7)
    List (SetMemory (d + 1, Rh), SetMemory (d, Rl), SetFlags (S = Some (Sf), V = Some (Vf),
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

object AND extends AvrInstructionObject[AND] {
  override val mask = 0xFC000000
  override val pattern = 0x20000000
  override protected def parse (buffer: Array[UnsignedByte]): AND = {
    val d = parseUnsignedParameter (buffer, 0x01F00000)
    val r = parseUnsignedParameter (buffer, 0x020F0000)
    new AND (d, r)
  }
}

class AND (val d: Int, val r: Int) extends Instruction[AvrCpu] {
  override def length = 2
  override def latency = 1
  override def execute (cpu: AvrCpu) = {
    val Rd = cpu.getMemory (d)
    val Rr = cpu.getMemory (r)
    val R = UnsignedByte (Rd.value & Rr.value)
    val Vf = false
    val Nf = R bit 7
    val Sf = Nf ^^ Vf
    val Zf = R == 0
    List (SetMemory (d, R), SetFlags (S = Some (Sf), V = Some (Vf), N = Some (Nf), Z = Some (Zf)))
  }
  override def toString = s"AND R${d}, R${r}"
}

object ANDI extends AvrInstructionObject[ANDI] {
  override val mask = 0xF0000000
  override val pattern = 0x70000000
  override protected def parse (buffer: Array[UnsignedByte]): ANDI = {
    val rawD = parseUnsignedParameter (buffer, 0x00F00000)
    val K = parseUnsignedParameter (buffer, 0x0F0F0000)
    new ANDI (rawD + 0x10, K)
  }
}

class ANDI (val d: Int, val K: UnsignedByte) extends Instruction[AvrCpu] {
  override def length = 2
  override def latency = 1
  override def execute (cpu: AvrCpu) = {
    val Rd = cpu.getMemory (d)
    val R = UnsignedByte (Rd.value & K.value)
    val Vf = false
    val Nf = R bit 7
    val Sf = Nf ^^ Vf
    val Zf = R == 0
    List (SetMemory (d, R), SetFlags (S = Some (Sf), V = Some (Vf), N = Some (Nf), Z = Some (Zf)))
  }
  override def toString = s"ANDI R${d}, $$${toHex (K, 2)}"
}

object BRBx extends AvrInstructionObject[BRBx] {
  override val mask = 0xF8000000
  override val pattern = 0xF0000000
  override protected def parse (buffer: Array[UnsignedByte]): BRBx = {
    val s = parseUnsignedParameter (buffer, 0x00070000)
    val set = parseUnsignedParameter (buffer, 0x04000000) == 0
    val unsignedK = parseUnsignedParameter (buffer, 0x03F80000)
    val k = if ((unsignedK & 0x40) == 0) unsignedK else (unsignedK | 0xFFFFFF80)
    new BRBx (s, set, k)
  }
}

class BRBx (val s: Int, val set: Boolean, val k: Int) extends Instruction[AvrCpu] {
  private var latencyOpt: Option[Int] = None
  override def length = 2
  override def latency = latencyOpt.get
  override def execute (cpu: AvrCpu) = {
    val sreg = cpu.getMemory (SREG).value
    val shouldBranchOn = if (set) {f: Int => (f != 0)} else {f: Int => (f == 0)}
    if (shouldBranchOn (sreg & (1 << s))) {
      latencyOpt = Some (2)
      List (IncrementIp (k * 2))
    }
    else {
      latencyOpt = Some (1)
      Nil
    }
  }
  override def toString = {
    val opcode = if (set) "BRBS" else "BRBC"
    val flagName = Flag.values ()(7 - s).name ()
    s"${opcode} '${flagName}', ${k}"
  }
}

object CALL extends AvrInstructionObject[CALL] {
  override val mask = 0xFE0E0000
  override val pattern = 0x940E0000
  override protected def parse (buffer: Array[UnsignedByte]): CALL = {
    val k = parseUnsignedParameter (buffer, 0x01F1FFFF)
    new CALL (k)
  }
}

class CALL (var k: Int) extends Instruction[AvrCpu] {
  override def length = 4
  override def latency = 5
  override def execute (cpu: AvrCpu) = {
    List (SetIp (k << 1))
  }
  override def toString = s"CALL $$${toHex (k, 6)}"
}

object CLx extends AvrInstructionObject[CLx] {
  override val mask = 0xFF8F0000
  override val pattern = 0x94880000
  override protected def parse (buffer: Array[UnsignedByte]): CLx = {
    val f = parseUnsignedParameter (buffer, 0x00700000)
    new CLx (f)
  }
}

class CLx (var f: Int) extends Instruction[AvrCpu] {
  override def length = 2
  override def latency = 1
  override def execute (cpu: AvrCpu) = {
    List (SetFlags (1 << f, 0x00))
  }
  override def toString = {
    val flag = Flag.values()(7 - f)
    s"CL${flag.name ()}"
  }
}

object COM extends AvrInstructionObject[COM] {
  override val mask = 0xFE0F0000
  override val pattern = 0x94000000
  override protected def parse (buffer: Array[UnsignedByte]): COM = {
    val d = parseUnsignedParameter (buffer, 0x01F00000)
    new COM (d)
  }
}

class COM (var d: Int) extends Instruction[AvrCpu] {
  override def length = 2
  override def latency = 1
  override def execute (cpu: AvrCpu) = {
    val Rd = cpu.getMemory (d)
    val R = 0xFF - Rd.value
    val Nf = (R & 0x80) != 0
    val Sf = Nf
    val Zf = R == 0
    List (SetMemory (d, R),
      SetFlags (S = Some (Sf), V = Some (false), N = Some (Nf), Z = Some (Zf), C = Some (true)))
  }
  override def toString = s"COM R${d}"
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

class CP (val d: Int, val r: Int) extends Instruction[AvrCpu] with AvrInstructionUtils {
  override def length = 2
  override def latency = 1
  override def execute (cpu: AvrCpu) = {
    val Rd = cpu.getMemory (d)
    val Rr = cpu.getMemory (r)
    val R = Rd - Rr
    val setFlags = builder (Rd, Rr, R).make ()
    List (setFlags)
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

class CPC (val d: Int, val r: Int) extends Instruction[AvrCpu] with AvrInstructionUtils {
  override def length = 2
  override def latency = 1
  override def execute (cpu: AvrCpu) = {
    val Rd = cpu.getMemory (d)
    val Rr = cpu.getMemory (r)
    val Cp = if (cpu.flag (Flag.C)) 1 else 0
    val R = Rd - Rr - Cp
    val setFlags = builder (Rd, Rr, R)
      .zero (if (R == 0) None else Some (false))
      .make ()
    List (setFlags)
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

class CPI (val d: Int, val K: UnsignedByte) extends Instruction[AvrCpu] with AvrInstructionUtils {
  override def length = 2
  override def latency = 1
  override def execute (cpu: AvrCpu) = {
    val Rd = cpu.getMemory (d)
    val R = Rd - K
    val setFlags = builder (Rd, K, R).make ()
    List (setFlags)
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
    val Rd = cpu.getMemory (d)
    val Rr = cpu.getMemory (r)
    if (Rd != Rr) {
      latencyOpt = Some (1)
      Nil
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
    0
  }

  private def handleTwoByteNextInstruction (): Int = {
    latencyOpt = Some (2)
    2
  }

  private def handleFourByteNextInstruction (): Int = {
    latencyOpt = Some (3)
    4
  }

  // TODO: There has to be a more elegant way of doing this.
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

object DEC extends AvrInstructionObject[DEC] {
  override val mask = 0xFE0F0000
  override val pattern = 0x940A0000
  override protected def parse (buffer: Array[UnsignedByte]): DEC = {
    val d = parseUnsignedParameter (buffer, 0x01F00000)
    new DEC (d)
  }
}

class DEC (val d: Int) extends Instruction[AvrCpu] with AvrInstructionUtils {
  override def length = 2
  override def latency = 1
  override def execute (cpu: AvrCpu) = {
    val Rd = cpu.getMemory (d)
    val R = UnsignedByte ((Rd.value - 1) & 0xFF)
    val setFlags = builder (Rd, UnsignedByte (1), R)
      .halfCarry (None)
      .carry (None)
      .overflow (Some (Rd.value == 0x80))
      .make ()
    List (SetMemory (d, R), setFlags)
  }
  override def toString = s"DEC R${d}"
}

object EIJMP extends AvrInstructionObject[EIJMP] {
  override val mask = 0xFFFF0000
  override val pattern = 0x94190000
  override protected def parse (buffer: Array[UnsignedByte]): EIJMP = {
    new EIJMP ()
  }
}

class EIJMP () extends Instruction[AvrCpu] {
  override def length = 2
  override def latency = 2
  override def execute (cpu: AvrCpu) = {
    val wordAddress = intFromBytes (cpu.portMap.readFromPort ("EIND"), cpu.getMemory (ZH), cpu.getMemory (ZL))
    List (SetIp (wordAddress << 1))
  }
  override def toString = "EIJMP"
}

object EOR extends AvrInstructionObject[EOR] {
  override val mask = 0xFC000000
  override val pattern = 0x24000000
  override protected def parse (buffer: Array[UnsignedByte]): EOR = {
    new EOR (parseUnsignedParameter (buffer, 0x01F00000), parseUnsignedParameter (buffer, 0x020F0000))
  }
}

class EOR (val d: Int, val r: Int) extends Instruction[AvrCpu] with AvrInstructionUtils {
  override def length = 2
  override def latency = 1
  override def execute (cpu: AvrCpu) = {
    val Rd = cpu.getMemory (d)
    val Rr = cpu.getMemory (r)
    val R = Rd ^ Rr
    val setFlags = builder (Rd, Rr, R)
      .halfCarry (None)
      .overflow (Some (false))
      .carry (None)
      .make ()
    List (SetMemory (d, R), setFlags)
  }
  override def toString = s"EOR R${d}, R${r}"
}

object IJMP extends AvrInstructionObject[IJMP] {
  override val mask = 0xFFFF0000
  override val pattern = 0x94090000
  override protected def parse (buffer: Array[UnsignedByte]): IJMP = {
    new IJMP ()
  }
}

class IJMP () extends Instruction[AvrCpu] {
  override def length = 2
  override def latency = 2
  override def execute (cpu: AvrCpu) = {
    val wordAddress = intFromBytes (cpu.getMemory (ZH), cpu.getMemory (ZL))
    List (SetIp (wordAddress << 1))
  }
  override def toString = "IJMP"
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
    val R = cpu.getMemory (A + 0x20)
    List (SetMemory (d, R))
  }
  override def toString = s"IN R${d}, $$${toHex (A, 2)}"
}

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
  override def toString = s"JMP $$${toHex (k, 4)}"
}

object LDD extends ComplexAvrInstructionObject[LDD] {
  val maskPatternPairs = List (
    (0xFE0F0000, 0x900C0000),
    (0xFE0F0000, 0x900D0000),
    (0xFE0F0000, 0x900E0000),

    (0xFE0F0000, 0x90090000),
    (0xFE0F0000, 0x900A0000),
    (0xD2080000, 0x80080000),

    (0xFE0F0000, 0x90010000),
    (0xFE0F0000, 0x90020000),
    (0xD2080000, 0x80000000)
  )

  override protected def parse (buffer: Array[UnsignedByte]): LDD = {
    val d = parseUnsignedParameter (buffer, 0x01F00000)
    val firstNybble = buffer(1).value >> 4
    val rDiscrim = buffer(0).value & 0x0C
    val xDiscrim = buffer(0).value & 0x03
    val (r, x) = (firstNybble, rDiscrim, xDiscrim) match {
      case (0x9, 0xC, 0x0) => ('X', Unchanged)
      case (0x9, 0xC, 0x1) => ('X', PostIncrement)
      case (0x9, 0xC, 0x2) => ('X', PreDecrement)
      case (0x9, 0x8, 0x1) => ('Y', PostIncrement)
      case (0x9, 0x8, 0x2) => ('Y', PreDecrement)
      case (0x9, 0x0, 0x1) => ('Z', PostIncrement)
      case (0x9, 0x0, 0x2) => ('Z', PreDecrement)
      case _ => parseUnsignedParameter (buffer, 0xD2080000) match {
        case 0x11 => ('Y', Unchanged)
        case 0x10 => ('Z', Unchanged)
        case _ => TEST_DRIVE_ME
      }
    }
    val q = (r, x, parseUnsignedParameter (buffer, 0x2C070000)) match {
      case ('X', _, _) => 0
      case (_, Unchanged, q) => q
      case _ => 0
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
    val initialValue = getExtended (cpu, regTuple)
    val preValue = x match {
      case IndirectionType.Unchanged => initialValue + q
      case IndirectionType.PostIncrement => initialValue
      case IndirectionType.PreDecrement => initialValue - 1
    }
    val R = cpu.getMemory (preValue)
    val postValue = x match {
      case IndirectionType.Unchanged => initialValue
      case IndirectionType.PostIncrement => preValue + 1
      case IndirectionType.PreDecrement => preValue
    }
    val regMod = if (postValue != initialValue) setExtended (regTuple, postValue) else Nil
    List (SetMemory (d, R)) ++ regMod
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
  private def regTuple = {
    r match {
      case 'X' => Xfull
      case 'Y' => Yfull
      case 'Z' => Zfull
      case _ => TEST_DRIVE_ME
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
  override def execute (cpu: AvrCpu) = List (SetMemory (d, K))
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
    val K = cpu.getMemory (k)
    List (SetMemory (d, K))
  }
  override def toString = s"LDS R${d}, $$${toHex (k, 4)}"
}

object LPM extends ComplexAvrInstructionObject[LPM] {
  override val maskPatternPairs = List (
    (0xFFEF0000, 0x95C80000),
    (0xFE0C0000, 0x90040000)
  )
  override protected def parse (buffer: Array[UnsignedByte]): LPM = {
    parseUnsignedParameter (buffer, 0x0E000000) match {
      case 0x2 => parseCaseI (buffer)
      case _ => parseOtherCases (buffer)
    }
  }
  private def parseCaseI (buffer: Array[UnsignedByte]): LPM = {
    val d = 0
    val extended = parseUnsignedParameter (buffer, 0x00100000) != 0
    val increment = false
    new LPM (d, extended, increment)
  }
  private def parseOtherCases (buffer: Array[UnsignedByte]): LPM = {
    val d = parseUnsignedParameter (buffer, 0x01F00000)
    val extended = parseUnsignedParameter (buffer, 0x00020000) != 0
    val increment = parseUnsignedParameter (buffer, 0x00010000) != 0
    new LPM (d, extended, increment)
  }
}

class LPM (val d: Int, val extended: Boolean, val increment: Boolean) extends Instruction[AvrCpu] {
  override def length = 2
  override def latency = 3
  override def execute (cpu: AvrCpu) = {
    val address = getExtended (cpu, Zfull)
    val R = cpu.programMemory (address)
    val handleZ = increment match {
      case false => Nil
      case true => {
        val newAddress = extended match {
          case false => (address & 0xFFFF0000) | ((address + 1) & 0xFFFF)
          case true => address + 1
        }
        setExtended (Zfull, newAddress)
      }
    }
    List (SetMemory (d, R)) ++ handleZ
  }
  override def toString = {
    val opcode = if (extended) "ELPM" else "LPM"
    val r = if (increment) "Z+" else "Z"
    s"${opcode} R${d}, ${r}"
  }
}

object MOV extends AvrInstructionObject[MOV] {
  override val mask = 0xFC000000
  override val pattern = 0x2C000000
  override protected def parse (buffer: Array[UnsignedByte]): MOV = {
    val d = parseUnsignedParameter (buffer, 0x01F00000)
    val r = parseUnsignedParameter (buffer, 0x020F0000)
    new MOV (d, r)
  }
}

class MOV (val d: Int, val r: Int) extends Instruction[AvrCpu] {
  override def length = 2
  override def latency = 1
  override def execute (cpu: AvrCpu) = {
    val Rr = cpu.getMemory (r)
    List (SetMemory (d, Rr))
  }
  override def toString = s"MOV R${d}, R${r}"
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
    val RrL = cpu.getMemory (r)
    val RrH = cpu.getMemory (r + 1)
    List (SetMemory (d, RrL), SetMemory (d + 1, RrH))
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
    val Rd: Int = cpu.getMemory (d)
    val Rr: Int = cpu.getMemory (r)
    val R = Rd * Rr
    val Cf = ((R & 0x8000) != 0)
    val Zf = (R == 0)
    List (SetMemory (1, ((R >> 8) & 0xFF)), SetMemory (0, (R & 0xFF)),
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
  override def execute (cpu: AvrCpu) = Nil
  override def toString = s"NOP"
}

object OR extends AvrInstructionObject[OR] {
  override val mask = 0xFC000000
  override val pattern = 0x28000000
  override protected def parse (buffer: Array[UnsignedByte]): OR = {
    val d = parseUnsignedParameter (buffer, 0x01F00000)
    val r = parseUnsignedParameter (buffer, 0x020F0000)
    new OR (d, r)
  }
}

class OR (val d: Int, val r: Int) extends Instruction[AvrCpu] {
  override def length = 2
  override def latency = 1
  override def execute (cpu: AvrCpu) = {
    val Rd = cpu.getMemory (d)
    val Rr = cpu.getMemory (r)
    val R = UnsignedByte (Rd.value | Rr.value)
    val Nf = (R bit 7)
    val Sf = Nf
    val Zf = (R == 0)
    List (SetMemory (d, R), SetFlags (S = Some (Sf), V = Some (false), N = Some (Nf), Z = Some (Zf)))
  }
  override def toString = s"OR R${d}, R${r}"
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
    val Rd = cpu.getMemory (d)
    val R = UnsignedByte (Rd.value | K)
    val Nf = (R bit 7)
    val Sf = Nf
    val Zf = (R == 0)
    List (SetMemory (d, R), SetFlags (S = Some (Sf), V = Some (false), N = Some (Nf), Z = Some (Zf)))
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
    val Rr = cpu.getMemory (r)
    List (SetMemory (A + 0x20, Rr.value))
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
    List (Pop (d))
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
    val R = cpu.getMemory (r)
    List (Push (R))
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
    List (PushIp (), IncrementIp (k * 2))
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
    List (PopIp (), IncrementIp (-2), SetFlags (I = Some (true)))
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
  override def execute (cpu: AvrCpu) = List (IncrementIp (k * 2))
  override def toString = s"RJMP ${k}"
}

object SBC extends AvrInstructionObject[SBC] {
  override val mask = 0xFC000000
  override val pattern = 0x08000000
  override protected def parse (buffer: Array[UnsignedByte]): SBC = {
    new SBC (parseUnsignedParameter (buffer, 0x01F00000), parseUnsignedParameter (buffer, 0x020F0000))
  }
}

class SBC (val d: Int, val r: Int) extends Instruction[AvrCpu] with AvrInstructionUtils {
  override def length = 2
  override def latency = 1
  override def execute (cpu: AvrCpu) = {
    val Rd = cpu.getMemory (d)
    val Rr = cpu.getMemory (r)
    val R: UnsignedByte = Rd - Rr - (if (cpu.flag (C)) 1 else 0)
    val setFlags = builder (Rd, Rr, R)
      .zero (if (R.value == 0) None else Some (false))
      .make ()
    List (SetMemory (d, R), setFlags)
  }
  override def toString = s"SBC R${d}, R${r}"
}

object SBCI extends AvrInstructionObject[SBCI] {
  override val mask = 0xF0000000
  override val pattern = 0x40000000
  override protected def parse (buffer: Array[UnsignedByte]): SBCI = {
    val rawD = parseUnsignedParameter (buffer, 0x00F00000)
    val K = parseUnsignedParameter (buffer, 0x0F0F0000)
    new SBCI (rawD + 0x10, K)
  }
}

class SBCI (val d: Int, val K: UnsignedByte) extends Instruction[AvrCpu] with AvrInstructionUtils {
  override def length = 2
  override def latency = 1
  override def execute (cpu: AvrCpu) = {
    val Rd = cpu.getMemory (d)
    val C = if (cpu.flag (Flag.C)) 1 else 0
    val R = Rd - K - C
    val setFlags = builder (Rd, K, R).make ()
    List (SetMemory (d, R), setFlags)
  }
  override def toString = s"SBCI R${d}, $$${toHex (K, 2)}"
}

object SBIS extends AvrInstructionObject[SBIS] {
  override val mask = 0xFF000000
  override val pattern = 0x9B000000
  override protected def parse (buffer: Array[UnsignedByte]): SBIS = {
    val A = parseUnsignedParameter (buffer, 0x00F80000)
    val b = parseUnsignedParameter (buffer, 0x00070000)
    new SBIS (A, b)
  }
}

class SBIS (val A: Int, val b: Int) extends Instruction[AvrCpu] {
  private var _latencyOpt: Option[Int] = None
  override def length = 2
  override def latency = _latencyOpt.get
  override def execute (cpu: AvrCpu): List[Event] = {
    val ioValue = cpu.getMemory (A + 0x20).value
    val bitIsSet = (ioValue & (1 << b)) != 0
    if (!bitIsSet) {
      _latencyOpt = Some (1)
      Nil
    }
    else {
      val nextInstruction = findNextInstruction (cpu)
      val length = nextInstruction.length
      _latencyOpt = Some ((nextInstruction.length / 2) + 1)
      List (IncrementIp (length))
    }
  }
  override def toString = s"SBIS $$${toHex (A, 2)}, ${b}"

  private def findNextInstruction (cpu: AvrCpu): Instruction[AvrCpu] = {
    val buffer = cpu.programMemory.getData (cpu.ip + 2, 4)
    new AvrInstructionSet ().apply (buffer).get
  }
}

object SBIW extends AvrInstructionObject[SBIW] {
  override val mask = 0xFF000000
  override val pattern = 0x97000000
  override protected def parse (buffer: Array[UnsignedByte]): SBIW = {
    val rawD = parseUnsignedParameter (buffer, 0x00300000)
    val K = parseUnsignedParameter (buffer, 0x00CF0000)
    new SBIW ((rawD << 1) + 24, K)
  }
}

class SBIW (val d: Int, val K: UnsignedByte) extends Instruction[AvrCpu] {
  override def length = 2
  override def latency = 2
  override def execute (cpu: AvrCpu) = {
    val Rdh = cpu.getMemory (d + 1)
    val Rdl = cpu.getMemory (d)
    val Rd = ((Rdh.value & 0xFF) << 8) | (Rdl.value & 0xFF)
    val R = Rd - K
    val Vf = (Rdh bit 7) && !((R & 0x1000) > 0)
    val Nf = (R & 0x1000) > 0
    val Sf = Nf ^^ Vf
    val Zf = R == 0
    val Cf = ((R & 0x1000) > 0) && !(Rdh bit 7)
    List (SetMemory (d + 1, (R >> 8) & 0xFF), SetMemory (d, R & 0xFF),
      SetFlags (S = Some (Sf), V = Some (Vf), N = Some (Nf), Z = Some (Zf), C = Some (Cf)))
  }
  override def toString = s"SBIW R${d + 1}:R${d}, $$${toHex (K, 2)}"
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
    List (SetFlags (1 << flagMask, 0xFF))
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
    val Rr = cpu.getMemory (r)
    val address = getExtended (cpu, Xfull)
    val preAddress = x.preOperate (address)
    val setMemory = SetMemory (preAddress, Rr)
    val postAddress = x.postOperate (preAddress)
    setMemory :: setExtended (Xfull, postAddress)
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
    val R = cpu.getMemory (r)
    val postValue = x match {
      case IndirectionType.Unchanged => initialValue
      case IndirectionType.PostIncrement => preValue + 1
      case IndirectionType.PreDecrement => preValue
    }
    val regMod = if (postValue != initialValue) setExtended (regTuple, postValue) else Nil
    List (SetMemory (preValue, R)) ++ regMod
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
    val Rr = cpu.getMemory (r)
    List (SetMemory (k, Rr))
  }
  override def toString = s"STS $$${toHex (k, 2)}, R${r}"
}

object SUB extends AvrInstructionObject[SUB] {
  override val mask = 0xFC000000
  override val pattern = 0x18000000
  override protected def parse (buffer: Array[UnsignedByte]): SUB = {
    val d = parseUnsignedParameter (buffer, 0x01F00000)
    val r = parseUnsignedParameter (buffer, 0x020F0000)
    new SUB (d, r)
  }
}

class SUB (val d: Int, val r: Int) extends Instruction[AvrCpu] with AvrInstructionUtils {
  override def length = 2
  override def latency = 1
  override def execute (cpu: AvrCpu) = {
    val Rd = cpu.getMemory (d)
    val Rr = cpu.getMemory (r)
    val R = Rd - Rr
    val setFlags = builder (Rd, Rr, R).make ()
    List (SetMemory (d, R), setFlags)
  }
  override def toString = s"SUB R${d}, R${r}"
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

class SUBI (val d: Int, val K: UnsignedByte) extends Instruction[AvrCpu] with AvrInstructionUtils {
  override def length = 2
  override def latency = 1
  override def execute (cpu: AvrCpu) = {
    val Rd = cpu.getMemory (d)
    val R = Rd - K
    val setFlags = builder (Rd, K, R).make ()
    List (SetMemory (d, R), setFlags)
  }
  override def toString = s"SUBI R${d}, $$${toHex (K, 2)}"
}
