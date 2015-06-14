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
    subject.setExecutionLogger {log => System.out.println (log)}

    describe ("and handed the Blink .hex file") {
      subject.loadHex (getClass.getClassLoader.getResourceAsStream ("static/hex/Blink.cpp.hex"))

      describe ("and hooked to digital pin 13") {
        val sampler = subject.pinSampler ("D13")

        describe ("and run for three hundredths of a second") {
          pending
          subject.runForSeconds (0.030)

          it ("shows pin 13 going on and off appropriately") {
println (s"\n\nBlink history:\n${sampler.history}\n\n")
            assert (sampler.sampleAtSecond (0.005) === 5.0)
            assert (sampler.sampleAtSecond (0.015) === 0.0)
            assert (sampler.sampleAtSecond (0.025) === 5.0)
          }
        }
      }
    }
  }
}
