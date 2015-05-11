package storytests

import org.scalatest.path
import simulino.simulator.peripheral.PinSampler
import simulino.simulator.{SimulatorConfiguration, Simulator}

/**
 * Created by dnwiebe on 5/10/15.
 */
class BlinkTest extends path.FunSpec {
  describe ("A Simulator, configured as an ATmega2560") {
    val jsonStream = getClass.getClassLoader.getResourceAsStream ("configurations/ATmega2560.json")
    val configuration = SimulatorConfiguration (jsonStream)
    val subject = new Simulator (configuration)

    describe ("and handed the Blink .hex file") {
      subject.loadHex (getClass.getClassLoader.getResourceAsStream ("static/hex/Blink.cpp.hex"))

      describe ("and hooked to a PinSampler on pin 13") {
        val sampler = new PinSampler (13, configuration)
        subject.addSubscriber (sampler)

        describe ("and run for three seconds") {
          subject.runForSeconds (3.0)

          it ("shows pin 13 going on and off appropriately") {
            pending
            assert (sampler.sampleAtSecond (0.5) === 5.0)
            assert (sampler.sampleAtSecond (1.5) === 0.0)
            assert (sampler.sampleAtSecond (2.5) === 5.0)
          }
        }
      }
    }
  }
}
