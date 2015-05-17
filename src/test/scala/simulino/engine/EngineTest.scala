package simulino.engine

import org.scalatest.path
import simulino.utils.TestUtils._

import scala.util.Try

class EngineTest extends path.FunSpec {
  describe ("An Engine") {
    val subject = new Engine ()

    it ("begins at tick 0") {
      assert (subject.currentTick === 0L)
      assert (subject.nextTick === 1L)
    }

    class EmptyEvent () extends Event

    describe ("ticked over five times") {
      (1 to 5).foreach {i => subject.tick ()}

      it ("is on tick 5") {
        assert (subject.currentTick === 5L)
      }

      it ("will next tick at 6") {
        assert (subject.nextTick === 6L)
      }

      describe ("and asked to schedule an event for tick 4") {
        val result = Try {subject.schedule (new EmptyEvent (), 4)}

        it ("complains") {
          fails (result, new IllegalArgumentException ("Can't schedule an event for tick 4 at tick 5"))
        }
      }

      it ("and asked to schedule an event for tick 5, is happy") {
        subject.schedule (new EmptyEvent (), 5)
        // no exception thrown
      }
    }

    describe ("given an Event for tick 10") {
      val event = new EmptyEvent ()
      subject.schedule (event, 10L)

      describe ("and a Subscriber to listen for it") {
        val subscriber = new Subscriber {
          var lastReceived: Option[Event] = None
          override def receive = {case e => lastReceived = Some(e)}
        }
        subject.addSubscriber (subscriber)

        describe ("and ticked ten times") {
          (1 to 10).foreach {i => subject.tick ()}

          it ("has not yet delivered the event") {
            assert (subscriber.lastReceived === None)
          }

          describe ("and then ticked once more") {
            subject.tick ()

            it ("delivers the event") {
              assert (subscriber.lastReceived === Some(event))
            }

            describe ("and ticked past 10 after the subscriber has been reset") {
              subscriber.lastReceived = None
              subject.tick ()

              it ("does not re-deliver the event") {
                assert (subscriber.lastReceived === None)
              }
            }
          }
        }

        describe ("and also events at ticks 5 and 7") {
          val firstEvent = new EmptyEvent ()
          subject.schedule (firstEvent, 5L)
          val secondEvent = new EmptyEvent ()
          subject.schedule (secondEvent, 7L)

          describe ("and ticked five times") {
            (1 to 5).foreach {i => subject.tick ()}

            it ("has not yet delivered the first event") {
              assert (subscriber.lastReceived === None)
            }

            describe ("and a sixth time") {
              subject.tick ()

              it ("has now delivered the first event") {
                assert (subscriber.lastReceived === Some(firstEvent))
              }

              describe ("and a seventh and eighth time") {
                subject.tick ()
                subject.tick ()

                it ("has now delivered the second event") {
                  assert (subscriber.lastReceived === Some (secondEvent))
                }

                describe ("and a ninth, tenth, and eleventh") {
                  (1 to 3).foreach {i => subject.tick ()}

                  it ("has finally delivered the original event") {
                    assert (subscriber.lastReceived === Some (event))
                  }
                }
              }
            }
          }
        }
      }
    }

    class PickySubscriber (target: Event) extends Subscriber {
      var fired = false
      override def receive = {
        case e: Event if e == target => fired match {
          case false => fired = true
          case true => fail ("Subscriber already fired")
        }
        case e =>
      }
    }

    describe ("given two Events for tick 3 and two subscribers to listen for them") {
      val oneEvent = new EmptyEvent ()
      subject.schedule (oneEvent, 3L)
      val oneSubscriber = new PickySubscriber (oneEvent)
      subject.addSubscriber(oneSubscriber)
      val anotherEvent = new EmptyEvent ()
      subject.schedule (anotherEvent, 3L)
      val anotherSubscriber = new PickySubscriber (anotherEvent)
      subject.addSubscriber(anotherSubscriber)

      describe ("and ticked past 3") {
        (1 to 4).foreach {i => subject.tick ()}

        it ("fires one subscriber") {
          assert (oneSubscriber.fired === true)
        }

        it ("fires the other subscriber") {
          assert (anotherSubscriber.fired === true)
        }
      }
    }
  }
}