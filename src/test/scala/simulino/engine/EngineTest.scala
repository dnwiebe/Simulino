package simulino.engine

import org.scalatest.path

class EngineTest extends path.FunSpec {
  describe ("An Engine") {
    val subject = new Engine ()

    it ("begins at tick 0") {
      assert (subject.nextTick === 0L)
    }

    class EmptyEvent (val tick: Long) extends Event

    describe ("ticked over five times") {
      (1 to 5).foreach {i => subject.tick ()}

      it ("will next tick at 5") {
        assert (subject.nextTick === 5L)
      }

      it ("and asked to schedule an event for tick 4, complains") {
        try {
          subject.schedule (new EmptyEvent (4))
          fail ()
        }
        catch {
          case e: IllegalArgumentException => assert (e.getMessage == "Can't schedule an event for tick 4 at tick 5")
        }
      }
    }

    describe ("given an Event for tick 10") {
      val event = new EmptyEvent (10L)
      subject.schedule (event)

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
          val firstEvent = new EmptyEvent (5L)
          subject.schedule (firstEvent)
          val secondEvent = new EmptyEvent (7L)
          subject.schedule (secondEvent)

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
      val oneEvent = new EmptyEvent (3L)
      subject.schedule (oneEvent)
      val oneSubscriber = new PickySubscriber (oneEvent)
      subject.addSubscriber(oneSubscriber)
      val anotherEvent = new EmptyEvent (3L)
      subject.schedule (anotherEvent)
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