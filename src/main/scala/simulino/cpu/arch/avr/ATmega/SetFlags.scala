package simulino.cpu.arch.avr.ATmega

import simulino.cpu.CpuChange
import simulino.engine.Event

/**
 * Created by dnwiebe on 5/13/15.
 */

case object SetFlags {

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

  def this (
    I: Option[Boolean] = None,
    T: Option[Boolean] = None,
    H: Option[Boolean] = None,
    S: Option[Boolean] = None,
    V: Option[Boolean] = None,
    N: Option[Boolean] = None,
    Z: Option[Boolean] = None,
    C: Option[Boolean] = None
  ) {
    this (
      SetFlags.makeMask (List (I, T, H, S, V, N, Z, C)),
      SetFlags.makePattern (List (I, T, H, S, V, N, Z, C))
    )
  }

  override def toString: String = {
    val names = Flag.values ()
    "SetFlags(" + (0 until names.length).map {i =>
      ((mask >> i) & 1, (pattern >> i) & 1) match {
        case (0, _) => "_"
        case (1, 1) => names(i).name
        case (1, 0) => names(i).name.toLowerCase
      }
    }.mkString ("") + ")"
  }
}