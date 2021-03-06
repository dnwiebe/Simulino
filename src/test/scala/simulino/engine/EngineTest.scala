package simulino.engine

import org.scalatest.path
import simulino.utils.TestUtils._
import org.mockito.Mockito._
import org.mockito.Matchers

import scala.util.Try

class EngineTest extends path.FunSpec {
  describe ("An Engine") {
    val subject = new Engine ()

    it ("begins at tick 0") {
      assert (subject.currentTick === 0L)
      assert (subject.nextTick === 1L)
    }

    describe ("given a TickSink") {
      val tickSink = mock (classOf[TickSink])
      subject.addTickSink (tickSink)

      describe ("and ticked over twice") {
        subject.tick ()
        subject.tick ()

        it ("calls the TickSink twice") {
          verify (tickSink).tick (0L)
          verify (tickSink).tick (1L)
        }

        describe ("and with the TickSink removed") {
          subject.removeTickSink (tickSink)

          describe ("and ticked over twice") {
            subject.tick ()
            subject.tick ()

            it ("the TickSink has still only been ticked twice") {
              verify (tickSink, times (2)).tick (Matchers.anyLong ())
            }
          }
        }
      }
    }

    class EmptyEvent () extends Event

    trait SelfEvent extends Event {
      def execute (engine: Engine): Unit
    }

    case class LazyEvent () extends SelfEvent {
      override def execute (engine: Engine): Unit = {}
    }

    case class GeneratorEvent (tick: Long) extends SelfEvent {
      override def execute (engine: Engine): Unit = {
        engine.schedule (new LazyEvent (), tick)
      }
    }

    class SelfSubscriber (engine: Engine) extends Subscriber {
      override def receive = {
        case e: SelfEvent => e.execute (engine)
        case _ => throw new UnsupportedOperationException (s"Can't handle ${getClass.getSimpleName}")
      }
    }

    describe ("given a Subscriber") {
      subject.addSubscriber (new SelfSubscriber (subject))

      describe ("and a LazyEvent") {
        subject.schedule (new LazyEvent (), subject.currentTick)

        describe ("and ticked") {
          subject.tick ()

          it ("the schedule is empty") {
            assert (subject.events.isEmpty === true)
          }
        }

        describe ("and a GeneratorEvent aimed at the next tick") {
          val currentTick = subject.currentTick
          subject.schedule (new GeneratorEvent (subject.nextTick), currentTick)

          describe ("and ticked") {
            subject.tick ()

            it ("the schedule contains only the one generated EmptyEvent") {
              assert (subject.events === List (ScheduledEvent (currentTick + 1, LazyEvent ())))
            }
          }
        }

        describe ("and a GeneratorEvent aimed at the current tick") {
          val currentTick = subject.currentTick
          subject.schedule (new GeneratorEvent (currentTick), currentTick)

          describe ("and ticked") {
            subject.tick ()

            it ("the schedule is empty") {
              assert (subject.events.isEmpty === true)
            }
          }
        }
      }
    }

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

    class PickySubscriber (name: String, engine: Engine, target: Event) extends Subscriber {
      var fired = false
      override def receive = {
        case e: Event if e == target => fired match {
          case false => fired = true
          case true => fail (s"At tick ${engine.currentTick}, subscriber ${name} has already fired")
        }
        case e =>
      }
    }

    describe ("given two Events for tick 3 and two subscribers to listen for them") {
      val oneEvent = new EmptyEvent ()
      subject.schedule (oneEvent, 3L)
      val oneSubscriber = new PickySubscriber ("oneSubscriber", subject, oneEvent)
      subject.addSubscriber(oneSubscriber)
      val anotherEvent = new EmptyEvent ()
      subject.schedule (anotherEvent, 3L)
      val anotherSubscriber = new PickySubscriber ("anotherSubscriber", subject, anotherEvent)
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