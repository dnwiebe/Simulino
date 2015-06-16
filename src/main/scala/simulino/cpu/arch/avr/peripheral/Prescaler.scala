package simulino.cpu.arch.avr.peripheral

import simulino.cpu.arch.avr.PortHandler
import simulino.engine.TickSink

/**
 * Created by dnwiebe on 6/15/15.
 */
class Prescaler extends PortHandler with TickSink {
  override val name = "Prescaler"
  override val portNames = List ("PSRSYNC")
  private var listMap: Map[Int, List[TickSink]] = List (8, 64, 256, 1024).map {(_, List[TickSink] ())}.toMap
  private var counter = 0

  def addSubscriber (subscriber: TickSink, multiplier: Int): Unit = {
    if (!listMap.contains (multiplier)) {
      val list = listMap.keys.toList.sorted
      throw new IllegalArgumentException (s"Prescaler only supports multipliers ${list.mkString (", ")}; not 47")
    }
    val prevList = listMap(multiplier)
    listMap = listMap + (multiplier -> (subscriber :: prevList))
  }

  def removeSubscriber (subscriber: TickSink): Unit = {
    val containingKeys = listMap.keys.filter {key => listMap (key).contains (subscriber)}
    containingKeys.foreach {key =>
      val prevList = listMap (key)
      listMap = listMap + (key -> prevList.filter {_ ne subscriber})
    }
  }

  override def tick (count: Long): Unit = {
    listMap.keys.foreach {key =>
      if ((counter & (key - 1)) == 0) {tickSubscribers (listMap (key), count)}
    }
    counter = (counter + 1) % 1024
  }

  private def tickSubscribers (subscribers: List[TickSink], count: Long): Unit = {
    subscribers.foreach {_.tick (count)}
  }
}
