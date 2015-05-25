package simulino.engine

/**
 * Created by dnwiebe on 5/25/15.
 */
trait TickSink {
  def tick (count: Long): Unit
}
