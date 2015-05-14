package simulino.cpu.arch.ATmega

import simulino.engine.Event

/**
 * Created by dnwiebe on 5/13/15.
 */
case class SetFlags (
  I: Option[Boolean] = None,
  T: Option[Boolean] = None,
  H: Option[Boolean] = None,
  S: Option[Boolean] = None,
  V: Option[Boolean] = None,
  N: Option[Boolean] = None,
  Z: Option[Boolean] = None,
  C: Option[Boolean] = None
) extends Event {

  override def toString: String = {
    val names = Flag.values ()
    val values = Array (I, T, H, S, V, N, Z, C)
    "SetFlags(" + (0 until names.length).map {i =>
      values(i) match {
        case None => "_"
        case Some (true) => names(i).name
        case Some (false) => names(i).name.toLowerCase
      }
    }.mkString ("") + ")"
  }
}