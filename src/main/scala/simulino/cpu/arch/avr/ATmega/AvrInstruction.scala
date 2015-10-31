package simulino.cpu.arch.avr.ATmega

import simulino.cpu.arch.avr.{SetFlags, AvrCpu}
import simulino.cpu.{Instruction, InstructionObject}
import simulino.memory.UnsignedByte

/**
 * Created by dnwiebe on 5/19/15.
 */

trait AvrInstructionObject[T <: Instruction[AvrCpu]] extends InstructionObject[T] {

  override protected def bufferToInt (buffer: Array[UnsignedByte]): Int = {
    List (1, 0, 3, 2).foldLeft (0) {(soFar, i) => i < buffer.length match {
      case true => (soFar << 8) | buffer(i).value
      case false => soFar << 8
    }}
  }
}

trait ConfigurableAvrInstructionObject {
  var latencyOpt: Option[Int] = None
}

trait ComplexAvrInstructionObject[T <: Instruction[AvrCpu]] extends AvrInstructionObject[T]{
  final override val mask = 0xFFFFFFFF // not used
  final override val pattern = 0xFFFFFFFF // not used
  protected val maskPatternPairs: List[(Int, Int)]

  override def apply (buffer: Array[UnsignedByte]): Option[T] = {
    val matches = maskPatternPairs.flatMap {pair =>
      val (mask, pattern) = pair
      if (matchOpcodePattern (buffer, mask, pattern)) Some (parse (buffer)) else None
    }
    matches match {
      case Nil => None
      case x :: Nil => Some (x)
      case _ => {
        val strBuffer = buffer.mkString(" ")
        val strInstructions = matches.mkString ("(\"", "\", \"", "\")")
        throw new IllegalStateException (s"Internal error: ambiguous buffer (${strBuffer}): could be any of ${strInstructions}")
      }
    }
  }
}

trait AvrInstructionUtils {

  class SetFlagsBuilder (dest: UnsignedByte, src: UnsignedByte, result: UnsignedByte) {
    private var H: Option[Boolean] = null
    private var S: Option[Boolean] = null
    private var V: Option[Boolean] = null
    private var N: Option[Boolean] = null
    private var Z: Option[Boolean] = null
    private var C: Option[Boolean] = null
    private val u = new AvrInstructionUtils () {}

    def halfCarry (v: Option[Boolean]): SetFlagsBuilder = {this.H = v; this}
    def sign (v: Option[Boolean]): SetFlagsBuilder = {this.S = v; this}
    def overflow (v: Option[Boolean]): SetFlagsBuilder = {this.V = v; this}
    def negative (v: Option[Boolean]): SetFlagsBuilder = {this.N = v; this}
    def zero (v: Option[Boolean]): SetFlagsBuilder = {this.Z = v; this}
    def carry (v: Option[Boolean]): SetFlagsBuilder = {this.C = v; this}

    def make (): SetFlags = {
      val n = h(N, u.negative (result))
      val v = h(V, u.overflow (dest, src, result))
      SetFlags (
        None,
        None,
        h(H, u.halfCarry (dest, src, result)),
        if (S == null) calculateSign (n, v) else S,
        if (V == null) v else V,
        if (N == null) n else N,
        h(Z, u.zero (result)),
        h(C, u.fullCarry (dest, src, result))
      )
    }

    private def h (prev: Option[Boolean], default: Boolean): Option[Boolean] = {
      if (prev == null) Some (default) else prev
    }

    private def calculateSign (N: Option[Boolean], V: Option[Boolean]): Option[Boolean] = {
      (N, V) match {
        case (None, _) => None
        case (_, None) => None
        case (Some (n), Some (v)) => Some (n ^ v)
      }
    }
  }

  def builder (dest: UnsignedByte, src: UnsignedByte, result: UnsignedByte): SetFlagsBuilder = {
    new SetFlagsBuilder (dest, src, result);
  }

  def halfCarry (dest: UnsignedByte, src: UnsignedByte, result: UnsignedByte): Boolean = {
    carry (dest bit 3, src bit 3, result bit 3)
  }

  def fullCarry (dest: UnsignedByte, src: UnsignedByte, result: UnsignedByte): Boolean = {
    carry (dest bit 7, src bit 7, result bit 7)
  }

  def negative (result: UnsignedByte): Boolean = {
    result bit 7
  }

  def zero (result: UnsignedByte): Boolean = {
    result.value == 0
  }

  def overflow (dest: UnsignedByte, src: UnsignedByte, result: UnsignedByte): Boolean = {
    val d7 = dest bit 7
    val s7 = src bit 7
    val r7 = result bit 7
    (d7 && !s7 && !r7) || (!d7 && s7 && r7)
  }

  private def carry (dest: Boolean, src: Boolean, result: Boolean): Boolean = {
    (!dest && src) || (src && result) || (result && !dest)
  }
}
