package simulino.cpu.arch.avr.peripheral

import org.scalatest.path
import org.mockito.Mockito._
import simulino.cpu.arch.avr.AvrCpu

/**
 * Created by dnwiebe on 6/12/15.
 */
class PinPortHandlerTest extends path.FunSpec {
  describe ("A PinPortHandler") {
    val subject = new PinPortHandler ("X")

    it ("has the right name") {
      assert (subject.name === "Port X")
    }

    it ("requests the correct ports") {
      val expectedPortNames = (0 until 7).foldLeft (Set[String] ()) {(soFar, idx) =>
        soFar + s"PINX${idx}" + s"DDX${idx}" + s"PORTX${idx}"
      }
      assert (subject.portNames.toSet === expectedPortNames)
    }

    describe ("given a mock AvrCpu") {
      val cpu = mock (classOf[AvrCpu])
      subject.initialize (cpu)
    }
  }
}
