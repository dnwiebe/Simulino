package simulino.engine

import scala.collection.mutable.ListBuffer
import simulino.utils.Utils._

/**
 * Created by dnwiebe on 5/8/15.
 */
class Engine {
  private var _currentTick: Long = 0L
  val events = new ListBuffer[ScheduledEvent] ()
  private var tickSinks: List[TickSink] = Nil
  private var subscribers: List[Subscriber] = Nil

  def tick (): Unit = {
    var clean = false
    while (!clean) {
      val currentEvents = events.takeWhile { event => event.tick == currentTick }
      if (currentEvents.isEmpty) {
        clean = true
      }
      else {
        events.remove (0, currentEvents.size)
        currentEvents.foreach (handle)
      }
    }
    tickSinks.foreach {_.tick (currentTick)}
    _currentTick += 1
  }

  def currentTick: Long = _currentTick
  def nextTick: Long = currentTick + 1L

  def schedule (event: Event, tick: Long): Unit = {
    schedule (ScheduledEvent (tick, event))
  }

  def schedule (event: ScheduledEvent): Unit = {
    validateSchedule (event)
    (0 until events.size).find {i => events(i).tick > event.tick} match {
      case None => events.append (event)
      case Some (index) => events.insert (index, event)
    }
  }

  def addTickSink (tickSink: TickSink): Unit = {
    tickSinks = tickSink :: tickSinks
  }

  def removeTickSink (tickSink: TickSink): Unit = {
    tickSinks = tickSinks.filterNot {_ eq tickSink}
  }

  def addSubscriber (subscriber: Subscriber): Unit = {
    subscribers = subscriber :: subscribers
  }

  private def handle (event: ScheduledEvent): Unit = {
    subscribers.foreach {s => s.receive (event.get)}
  }

  private def validateSchedule (event: ScheduledEvent): Unit = {
    if (event.tick < currentTick) {
      throw new IllegalArgumentException (s"Can't schedule an event for tick ${event.tick} at tick ${currentTick}")
    }
  }
}
