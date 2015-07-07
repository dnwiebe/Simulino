package simulino.cpu.arch.avr

import simulino.cpu.arch.avr.ATmega.Flag
import simulino.cpu.CpuChange
import simulino.memory.{Span, UnsignedByte}
import simulino.utils.Utils._

/**
 * Created by dnwiebe on 6/7/15.
 */
case class PushIp () extends CpuChange[AvrCpu] {

  override def execute (cpu: AvrCpu): Unit = {
    val nextIp = cpu.ip + 2
    val firstByte = (nextIp >> 16) & 0xFF
    val secondByte = (nextIp >> 8) & 0xFF
    val thirdByte = nextIp & 0xFF
    val data = Array(UnsignedByte (firstByte), UnsignedByte (secondByte), UnsignedByte (thirdByte))
    cpu.dataMemory.addSpan (Span (cpu.sp - 2, data))
    cpu.sp = cpu.sp - 3
  }

  override def mods (cpu: AvrCpu): String = {
    val beforeIp = cpu.ip
    val afterHigh = (beforeIp >> 16) & 0xFF
    val afterMiddle = (beforeIp >> 8) & 0xFF
    val afterLow = (beforeIp >> 0) & 0xFF
    val beforeSp = cpu.sp
    val beforeHigh = cpu.getMemory (beforeSp - 0).value
    val beforeMiddle = cpu.getMemory (beforeSp - 1).value
    val beforeLow = cpu.getMemory (beforeSp - 2).value
    val afterSp = beforeSp - 3
    val firstXfer = s"($$${toHex (beforeSp - 0, 4)}): $$${toHex (beforeLow, 2)} -> $$${toHex (afterLow, 2)}"
    val secondXfer = s"($$${toHex (beforeSp - 1, 4)}): $$${toHex (beforeMiddle, 2)} -> $$${toHex (afterMiddle, 2)}"
    val thirdXfer = s"($$${toHex (beforeSp - 2, 4)}): $$${toHex (beforeHigh, 2)} -> $$${toHex (afterHigh, 2)}"
    s"${firstXfer}; ${secondXfer}; ${thirdXfer}; SP: $$${toHex (beforeSp, 4)} -> $$${toHex (afterSp, 4)}"
  }
}

case class PopIp () extends CpuChange[AvrCpu] {

  override def execute (cpu: AvrCpu): Unit = {
    TEST_DRIVE_ME
  }

  override def mods (cpu: AvrCpu): String = {
    val beforeIp = cpu.ip
    val beforeSp = cpu.sp
    val afterIp = RegisterNames.getExtended (cpu, (beforeSp + 1, beforeSp + 2, beforeSp + 3))
    val afterSp = beforeSp + 3
    s"IP: $$${toHex (beforeIp, 2)} -> $$${toHex (afterIp, 2)}; SP: $$${toHex (beforeSp, 4)} -> $$${toHex (afterSp, 4)}"
  }
}

case class Push (value: UnsignedByte) extends CpuChange[AvrCpu] {

  override def execute (cpu: AvrCpu): Unit = {
    cpu.dataMemory.update (cpu.sp, value)
    cpu.sp = cpu.sp - 1
  }

  override def mods (cpu: AvrCpu): String = {
    val beforeSp = cpu.sp
    val beforeValue = cpu.getMemory (beforeSp)
    val afterSp = beforeSp - 1
    s"($$${toHex (beforeSp, 4)}): $$${beforeValue} -> $$${value}; SP: $$${toHex (beforeSp, 4)} -> $$${toHex (afterSp, 4)}"
  }
}

case class Pop (address: Int) extends CpuChange[AvrCpu] {

  override def execute (cpu: AvrCpu): Unit = {
    TEST_DRIVE_ME
  }

  override def mods (cpu: AvrCpu): String = {
    val beforeSp = cpu.sp
    val afterSp = beforeSp + 1
    val beforeValue = cpu.getMemory (address)
    val value = cpu.getMemory (afterSp)
    s"($$${toHex (address, 2)}): $$${beforeValue} -> $$${value}; SP: $$${toHex (beforeSp, 4)} -> $$${toHex (afterSp, 4)}"
  }
}

case class SetSp (newSp: Int) extends CpuChange[AvrCpu] {

  override def execute (cpu: AvrCpu): Unit = {
    cpu.sp = newSp
  }

  override def mods (cpu: AvrCpu): String = {
    val beforeSp = cpu.sp
    s"SP: $$${toHex (beforeSp, 4)} -> $$${toHex (newSp, 4)}"
  }
}

case class SetMemory (address: Int, value: UnsignedByte) extends CpuChange[AvrCpu] {

  override def execute (cpu: AvrCpu): Unit = {
    cpu.setMemory (address, value)
  }

  override def mods (cpu: AvrCpu): String = {
    val oldValue = cpu.getMemory (address)
    s"($$${toHex (address, 2)}): $$${oldValue} -> $$${value}"
  }
}


case object SetFlags {

  def apply (
    I: Option[Boolean] = None,
    T: Option[Boolean] = None,
    H: Option[Boolean] = None,
    S: Option[Boolean] = None,
    V: Option[Boolean] = None,
    N: Option[Boolean] = None,
    Z: Option[Boolean] = None,
    C: Option[Boolean] = None
  ): SetFlags = {
    val list = List (I, T, H, S, V, N, Z, C)
    val mask = makeMask (list)
    val pattern = makePattern (list)
    apply (mask, pattern)
  }

  private def makeMask (bits: List[Option[Boolean]]): Int = {
    bits.foldLeft (0) {(soFar, bitOpt) =>
      val shifted = soFar << 1
      bitOpt match {
        case Some(x) => shifted | 1
        case None => shifted | 0
      }
    }
  }

  private def makePattern (bits: List[Option[Boolean]]): Int = {
    bits.foldLeft (0) {(soFar, bitOpt) =>
      val shifted = soFar << 1
      bitOpt match {
        case Some (true) => shifted | 1
        case _ => shifted | 0
      }
    }
  }
}

case class SetFlags (mask: Int, pattern: Int) extends CpuChange[AvrCpu] {

  var I = flag (mask, pattern, 7)
  var T = flag (mask, pattern, 6)
  var H = flag (mask, pattern, 5)
  var S = flag (mask, pattern, 4)
  var V = flag (mask, pattern, 3)
  var N = flag (mask, pattern, 2)
  var Z = flag (mask, pattern, 1)
  var C = flag (mask, pattern, 0)

  override def execute (cpu: AvrCpu): Unit = {
    cpu.setMemory (RegisterNames.SREG, newValue (mask, pattern, cpu.getMemory (RegisterNames.SREG)))
  }

  override def mods (cpu: AvrCpu): String = {
    val beforeFlags = cpu.getMemory (RegisterNames.SREG).value
    val afterFlags = newValue (mask, pattern, beforeFlags)
    s"SREG: ${flagString (0xFF, beforeFlags)} -> ${flagString (0xFF, afterFlags)}"
  }

  override def toString: String = {
    "SetFlags(" + flagString (mask, pattern) + ")"
  }

  private def newValue (mask: Int, pattern: Int, oldValue: Int): Int = {
    val withSets = oldValue | (mask & pattern)
    val withSetsAndClears = withSets & (~mask | pattern)
    withSetsAndClears
  }

  private def flagString (mask: Int, pattern: Int): String = {
    val names = Flag.values ()
    (0 until names.length).map {i =>
      flag (mask, pattern, 7 - i) match {
        case None => "_"
        case Some (true) => names(i).name
        case Some (false) => names(i).name.toLowerCase
      }
    }.mkString ("")
  }

  private def flag (mask: Int, pattern: Int, bit: Int): Option[Boolean] = {
    ((mask >> bit) & 1, (pattern >> bit) & 1) match {
      case (0, _) => None
      case (1, 1) => Some (true)
      case (1, 0) => Some (false)
    }
  }
}

case class MaskInterruptsForNextInstruction () extends CpuChange[AvrCpu] {

  override def execute (cpu: AvrCpu): Unit = {
    TEST_DRIVE_ME
  }

  override def mods (cpu: AvrCpu): String = {
    ""
  }
}
