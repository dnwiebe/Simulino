package simulino.simulator.peripheral

import org.scalatest.path

/**
 * Created by dnwiebe on 5/11/15.
 */
class PinSamplerTest extends path.FunSpec {
  describe ("A PinSampler") {
    val subject = new PinSampler (1000)

    describe ("without any samples") {
      it ("shows tri-state everywhere") {
        assert (subject.sampleAtTick (-1000L) === None)
        assert (subject.sampleAtTick (0L) === None)
        assert (subject.sampleAtTick (1000L) === None)
      }
    }

    describe ("given a couple of samples") {
      subject.addSample (3000, Some (2.25))
      subject.addSample (1000, Some (4.5))
      subject.addSample (2000, Some (3.75))
      subject.addSample (1500, None)

      it ("shows the expected waveform") {
        assert (subject.sampleAtTick (0) === None)
        assert (subject.sampleAtTick (140) === None)
        assert (subject.sampleAtTick (999) === None)
        assert (subject.sampleAtTick (1000) === Some (4.5))
        assert (subject.sampleAtTick (1499) === Some (4.5))
        assert (subject.sampleAtTick (1500) === None)
        assert (subject.sampleAtTick (1999) === None)
        assert (subject.sampleAtTick (2000) === Some (3.75))
        assert (subject.sampleAtTick (2999) === Some (3.75))
        assert (subject.sampleAtTick (3000) === Some (2.25))
      }
    }

    describe ("given a rising edge at the one-second mark") {
      subject.addSample (0, Some (0.00))
      subject.addSample (1000, Some (5.00))

      it ("shows zero juuuust before 1s") {
        assert (subject.sampleAtSecond (0.9999999) === Some (0.0))
      }

      it ("shows voltage juuuust after 1s") {
        assert (subject.sampleAtSecond (1.0000001) === Some (5.0))
      }
    }
  }
}
