package simulino.cpu.arch.avr.ATmega

import simulino.cpu.CpuChange
import simulino.engine.Event

/**
 * Created by dnwiebe on 5/13/15.
 */

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

case class SetFlags (mask: Int, pattern: Int) extends CpuChange {

  var I = flag (7)
  var T = flag (6)
  var H = flag (5)
  var S = flag (4)
  var V = flag (3)
  var N = flag (2)
  var Z = flag (1)
  var C = flag (0)

  override def toString: String = {
    val names = Flag.values ()
    "SetFlags(" + (0 until names.length).map {i =>
      flag (7 - i) match {
        case None => "_"
        case Some (true) => names(i).name
        case Some (false) => names(i).name.toLowerCase
      }
    }.mkString ("") + ")"
  }

  private def flag (bit: Int): Option[Boolean] = {
    ((mask >> bit) & 1, (pattern >> bit) & 1) match {
      case (0, _) => None
      case (1, 1) => Some (true)
      case (1, 0) => Some (false)
    }
  }
}