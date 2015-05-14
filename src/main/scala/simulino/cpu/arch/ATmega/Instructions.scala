package simulino.cpu.arch.ATmega

import simulino.cpu.arch.AvrCpu
import simulino.cpu.{IncrementIp, Instruction, InstructionObject}
import simulino.memory.UnsignedByte
import simulino.cpu.Implicits.RegisterInt
import simulino.cpu.Implicits.RegisterBit

/**
  * Created by dnwiebe on 5/12/15.
  */

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
    new RJMP (((buffer(0).value & 0x0F) << 8) + buffer(1).value)
  }
}

class RJMP (val k: Int) extends Instruction[AvrCpu] {
  override def length = 2
  override def latency = 2
  override def execute (cpu: AvrCpu) = List (IncrementIp (k * 2))
}


object SBC extends InstructionObject[SBC] {
  override val mask = 0xFC000000
  override val pattern = 0x08000000
  override protected def parse (buffer: Array[UnsignedByte]): SBC = {
    val r = ((buffer(0).value & 0x02) << 3) | (buffer(1).value & 0x0F)
    val d = ((buffer(0).value & 0x01) << 4) | (buffer(1).value >> 4)
    new SBC (d, r)
  }
}

class SBC (val d: Int, val r: Int) extends Instruction[AvrCpu] {
  override def length = 2
  override def latency = 1
  override def execute (cpu: AvrCpu) = {
    val Rd = cpu.register (d)
    val Rr = cpu.register (r)
    val R = Rd - Rr - (if (cpu.flag ('C)) 1 else 0)
    val Hf = (!(Rd bit 3) && (Rr bit 3)) || ((Rr bit 3) && (R bit 3)) || ((R bit 3) && !(Rd bit 3))
    val Vf = ((Rd bit 7) && !(Rr bit 7) && !(R bit 7)) || (!(Rd bit 7) && (Rr bit 7) && (R bit 7))
    val Nf = R bit 7
    val Sf = Nf ^^ Vf
    val Zfopt = if (R == 0) None else Some (false)
    val Cf = (!(Rd bit 7) && (Rr bit 7)) || ((Rr bit 7) && (R bit 7)) || ((R bit 7) && !(Rd bit 7))
    List (IncrementIp (2), SetRegister (d, R), SetFlags (H = Some (Hf), V = Some (Vf), N = Some (Nf),
      S = Some (Sf), Z = Zfopt, C = Some (Cf)))
  }
}
