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
) extends Event