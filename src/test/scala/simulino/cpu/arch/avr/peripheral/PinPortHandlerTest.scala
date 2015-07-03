package simulino.cpu.arch.avr.peripheral

import org.mockito.Matchers
import org.scalatest.path
import org.mockito.Mockito._
import simulino.cpu.arch.avr.{PortMap, AvrCpu}
import simulino.simulator.CpuConfiguration

/**
 * Created by dnwiebe on 6/12/15.
 */
class PinPortHandlerTest extends path.FunSpec {
  describe ("A PinPortHandler") {
    val subject = new PinPortHandler ("X", 3)

    it ("has the right name") {
      assert (subject.name === "Port X3")
    }

    it ("requests the correct ports") {
      assert (subject.portNames.toSet === Set ("DDX3", "PORTX3", "PINX3"))
    }

    describe ("given a mock AvrCpu and PortMap") {
      val cpu = mock (classOf[AvrCpu])
      val config = new CpuConfiguration (0, 7.3, null, null);
      when (cpu.config).thenReturn (config)
      val portMap = mock (classOf[PortMap])
      when (cpu.portMap).thenReturn (portMap)
      subject.initialize (cpu)

      describe ("if configured as an output") {
        subject.acceptChange ("DDX3", 0, 1)

        describe ("and a new one written to the port bit") {
          subject.acceptChange ("PORTX3", 0, 1)

          it ("the pin is set high") {
            verify (cpu).showVoltageAtPin("PX3", Some (7.3))
          }
        }

        describe ("and a one rewritten to the port bit") {
          subject.acceptChange ("PORTX3", 1, 1)

          it ("nothing changes") {
            verify (cpu, never).showVoltageAtPin(Matchers.any (classOf[String]), Matchers.any (classOf[Option[Double]]))
          }
        }

        describe ("and a new zero written to the port bit") {
          subject.acceptChange ("PORTX3", 1, 0)

          it ("the pin is set low") {
            verify (cpu).showVoltageAtPin("PX3", Some (0.0))
          }
        }

        describe ("and a zero rewritten to the port bit") {
          subject.acceptChange ("PORTX3", 0, 0)

          it ("nothing changes") {
            verify (cpu, never).showVoltageAtPin(Matchers.any (classOf[String]), Matchers.any (classOf[Option[Double]]))
          }
        }
      }

      describe ("if configured as an input") {
        subject.acceptChange ("DDX3", 1, 0)

        describe ("and a new one written to the port bit to turn on the pull-up resistor") {
          subject.acceptChange ("PORTX3", 0, 1)

          it ("the pin is set high") {
            verify (cpu).showVoltageAtPin("PX3", Some (7.3))
          }
        }

        describe ("and a one rewritten to the port bit") {
          subject.acceptChange ("PORTX3", 1, 1)

          it ("nothing changes") {
            verify (cpu, never).showVoltageAtPin(Matchers.any (classOf[String]), Matchers.any (classOf[Option[Double]]))
          }
        }

        describe ("and a new zero written to the port bit to turn off the pull-up resistor") {
          subject.acceptChange ("PORTX3", 1, 0)

          it ("the pin is tri-stated") {
            verify (cpu).showVoltageAtPin("PX3", None)
          }
        }

        describe ("and a zero rewritten to the port bit") {
          subject.acceptChange ("PORTX3", 0, 0)

          it ("nothing changes") {
            verify (cpu, never).showVoltageAtPin(Matchers.any (classOf[String]), Matchers.any (classOf[Option[Double]]))
          }
        }
      }
    }
  }
}
