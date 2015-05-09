package simulino.engine

/**
 * Created by dnwiebe on 5/8/15.
 */
trait Subscriber {
  def receive: PartialFunction[Event, Unit]
}
