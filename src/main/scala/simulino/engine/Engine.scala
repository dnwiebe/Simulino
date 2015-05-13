package simulino.engine

import scala.collection.mutable.ListBuffer

/**
 * Created by dnwiebe on 5/8/15.
 */
class Engine {
  private var currentTick = 0L
  private val events = new ListBuffer[ScheduledEvent] ()
  private var subscribers: List[Subscriber] = Nil

  def tick (): Unit = {
    val currentEvents = events.takeWhile {event => event.tick == nextTick}
    events.remove (0, currentEvents.size)
    currentEvents.foreach (handle)
    currentTick += 1
  }

  def nextTick = currentTick

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

  def addSubscriber (subscriber: Subscriber): Unit = {
    subscribers = subscriber :: subscribers
  }

  private def handle (event: ScheduledEvent): Unit = {
    subscribers.foreach {s => s.receive (event.get)}
  }

  private def validateSchedule (event: ScheduledEvent): Unit = {
    if (event.tick < nextTick) {
      throw new IllegalArgumentException (s"Can't schedule an event for tick ${event.tick} at tick ${nextTick}")
    }
  }
}
