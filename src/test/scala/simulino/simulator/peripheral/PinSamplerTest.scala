package simulino.simulator.peripheral

import org.mockito.Mockito._
import org.scalatest.path
import simulino.simulator.{CpuConfiguration, SimulatorConfiguration}
import simulino.simulator.events.PinVoltageChange

/**
 * Created by dnwiebe on 5/11/15.
 */
class PinSamplerTest extends path.FunSpec {
  describe ("A PinSampler") {
    val cpuConfig = mock (classOf[CpuConfiguration])
    when (cpuConfig.clockSpeed).thenReturn (1000)
    val subject = new PinSampler (42, new SimulatorConfiguration(cpu = cpuConfig))

    describe ("given a couple of events for it and a couple for something else") {
      subject.receive (new PinVoltageChange(3000, 42, 2.25))
      subject.receive (new PinVoltageChange(140, 43, 1.125))
      subject.receive (new PinVoltageChange(1000, 42, 4.5))
      subject.receive (new PinVoltageChange(1100, 41, 0.0))
      subject.receive (new PinVoltageChange(2000, 42, 3.75))

      it ("receives the ones intended for it and ignores the others") {
        assert (subject.sampleAtTick (0) === 0.0)
        assert (subject.sampleAtTick (140) === 0.0)
        assert (subject.sampleAtTick (999) === 0.0)
        assert (subject.sampleAtTick (1000) === 4.5)
        assert (subject.sampleAtTick (1100) === 4.5)
        assert (subject.sampleAtTick (1999) === 4.5)
        assert (subject.sampleAtTick (2000) === 3.75)
        assert (subject.sampleAtTick (2999) === 3.75)
        assert (subject.sampleAtTick (3000) === 2.25)
      }
    }

    describe ("given a rising edge at the one-second mark") {
      subject.receive (new PinVoltageChange (1000, 42, 5.00))

      it ("shows zero juuuust before 1s") {
        assert (subject.sampleAtSecond (0.9999999) === 0.0)
      }

      it ("shows zero juuuust after 1s") {
        assert (subject.sampleAtSecond (1.0000001) === 5.0)
      }
    }
  }
}
