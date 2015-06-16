package simulino.cpu.arch.avr.peripheral

import org.mockito.Matchers
import org.scalatest.path
import org.mockito.Mockito._
import simulino.cpu.arch.avr.AvrCpu
import simulino.engine.TickSink

import scala.util.Try
import simulino.utils.TestUtils._

/**
 * Created by dnwiebe on 6/15/15.
 */
class PrescalerTest extends path.FunSpec {
  describe ("A Prescaler") {
    val cpu = mock (classOf[AvrCpu])
    val subject = new Prescaler ()
    subject.initialize (cpu)

    it ("has the expected name") {
      assert (subject.name === "Prescaler")
    }

    it ("requests the proper ports") {
      assert (subject.portNames === List ("PSRSYNC"))
    }

    it ("does not complain about removing a nonexistent subscriber") {
      subject.removeSubscriber (mock (classOf[TickSink]))
    }

    describe ("given a subscriber with an unsupported multiplier") {
      val result = Try {subject.addSubscriber (null, 47)}

      it ("complains") {
        fails(result, new IllegalArgumentException ("Prescaler only supports multipliers 8, 64, 256, 1024; not 47"))
      }
    }

    describe ("given subscribers") {
      val subscriber8 = mock (classOf[TickSink])
      subject.addSubscriber (subscriber8, 8)
      val subscriber64 = mock (classOf[TickSink])
      subject.addSubscriber (subscriber64, 64)
      val subscriber256 = mock (classOf[TickSink])
      subject.addSubscriber (subscriber256, 256)
      val subscriber1024a = mock (classOf[TickSink])
      subject.addSubscriber (subscriber1024a, 1024)
      val subscriber1024b = mock (classOf[TickSink])
      subject.addSubscriber (subscriber1024b, 1024)

      describe ("and ticked 1024 times") {
        val offset = 23674L
        (0L until 1024L).foreach {count => subject.tick (count + offset)}

        it ("ticks subscribers the appropriate number of times") {
          tickedTimesStartingFrom (subscriber8, 128, offset)
          tickedTimesStartingFrom (subscriber64, 16, offset)
          tickedTimesStartingFrom (subscriber256, 4, offset)
          tickedTimesStartingFrom (subscriber1024a, 1, offset)
          tickedTimesStartingFrom (subscriber1024b, 1, offset)
        }
      }
    }

    describe ("given a subscriber for multiplier 8") {
      val subscriber = mock (classOf[TickSink])
      subject.addSubscriber (subscriber, 8)

      describe ("and ticked 8 times") {
        (0L until 8L).foreach (subject.tick)

        it ("ticks the subscriber once") {
          verify (subscriber, times (1)).tick (Matchers.anyLong ())
        }

        describe ("and then the subscriber is removed") {
          subject.removeSubscriber (subscriber)

          describe ("and ticked 8 more times") {
            (0L until 8L).foreach (subject.tick)

            it ("does not tick the subscriber again") {
              verify (subscriber, times (1)).tick (Matchers.anyLong ())
            }
          }
        }
      }
    }
  }

  private def tickedTimesStartingFrom (subscriber: TickSink, count: Int, offset: Long): Unit = {
    val step = 1024 / count
    val expectedTicks = (0 until count).foldLeft (List[Long] ()) {(soFar, idx) =>
      offset + (idx * step) :: soFar
    }.reverse
    expectedTicks.foreach (verify (subscriber).tick)
  }
}
