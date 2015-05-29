package simulino.cpu.arch

import simulino.cpu.arch.avr.ATmega.{ADD, SetMemory, SetFlags, Flag}
import simulino.cpu.arch.avr.ATmega.Flag._

import org.scalatest.path
import org.mockito.Mockito._
import simulino.cpu.IncrementIp
import simulino.cpu.arch.avr.AvrCpu
import simulino.engine.Engine
import simulino.memory.{UnsignedByte, Memory}
import simulino.simulator.{SimulatorConfiguration, CpuConfiguration}

/**
 * Created by dnwiebe on 5/14/15.
 */
class AvrCpuTest extends path.FunSpec {
  val config = SimulatorConfiguration (getClass.getClassLoader.getResourceAsStream ("configurations/ATmega2560.json")).cpu

  describe ("An AvrCpu with a mock Engine and Memory") {
    val engine = mock (classOf[Engine])
    when (engine.currentTick).thenReturn (1000)
    val memory = mock (classOf[Memory])

    val subject = new AvrCpu (engine, memory, config)

    it ("initially has zeros in all registers") {
      (0x00 until 0x200).foreach {i => assert (subject.register (i) === UnsignedByte (0), s"Register ${i}")}
    }

    it ("initially has zeros in all flags") {
      Flag.values ().foreach { f =>
        assert (subject.flag (f) === false, s"Flag ${f}")
      }
    }

    it ("has the expected interrupt vectors") {
      assert (subject.interruptVectors("RESET") === 0x00)
      assert (subject.interruptVectors("TIM0_COMPA") === 0x54)
      assert (subject.interruptVectors("TIM0_COMPB") === 0x58)
      assert (subject.interruptVectors("TIM0_OVF") === 0x5C)
    }

    describe ("directed to set R28 to 47") {
      subject.receive (SetMemory (28, 47))

      it ("does so") {
        (0 until 32).foreach {i =>
          assert (subject.register (i) === (if (i == 28) UnsignedByte (47) else UnsignedByte (0)))
        }
      }
    }

    describe ("directed to set all flags") {
      subject.receive (SetFlags (Some (true), Some (true), Some (true), Some (true),
        Some (true), Some (true), Some (true), Some (true)))

      it ("does so") {
        assert (subject.flag (I))
        assert (subject.flag (T))
        assert (subject.flag (H))
        assert (subject.flag (S))
        assert (subject.flag (V))
        assert (subject.flag (N))
        assert (subject.flag (Z))
        assert (subject.flag (C))
      }

      describe ("then to clear some flags") {
        subject.receive (SetFlags (I = Some (false), H = Some (false), V = Some (false), Z = Some (false)))

        it ("does so") {
          assert (!subject.flag (I))
          assert (subject.flag (T))
          assert (!subject.flag (H))
          assert (subject.flag (S))
          assert (!subject.flag (V))
          assert (subject.flag (N))
          assert (!subject.flag (Z))
          assert (subject.flag (C))
        }

        describe ("and then to swap all of them") {
          subject.receive (SetFlags (I = Some (true), T = Some (false), H = Some (true), S = Some (false),
            V = Some (true), N = Some (false), Z = Some (true), C = Some (false)))

          it ("does so") {
            assert (subject.flag (I))
            assert (!subject.flag (T))
            assert (subject.flag (H))
            assert (!subject.flag (S))
            assert (subject.flag (V))
            assert (!subject.flag (N))
            assert (subject.flag (Z))
            assert (!subject.flag (C))
          }

          describe ("and then to clear the rest") {
            subject.receive (SetFlags (I = Some (false), H = Some (false), V = Some (false), Z = Some (false)))

            it ("does so") {
              assert (!subject.flag (I))
              assert (!subject.flag (T))
              assert (!subject.flag (H))
              assert (!subject.flag (S))
              assert (!subject.flag (V))
              assert (!subject.flag (N))
              assert (!subject.flag (Z))
              assert (!subject.flag (C))
            }
          }
        }
      }
    }

    describe ("when set up with ones in Registers 0 and 1") {
      subject.setMemory(0, UnsignedByte (1))
      subject.setMemory(1, UnsignedByte (1))

      describe ("and instructed to add the contents of R1 to the contents of R0") {
        subject.receive (new ADD (0, 1))

        describe ("schedules the correct events for the correct clock tick") {
          verify (engine).schedule (IncrementIp (2), 1001)
          verify (engine).schedule (SetMemory (0, 2), 1001)
          verify (engine).schedule (SetFlags (None, None, Some (false), Some (false), Some (false), Some (false),
            Some (false), Some (false)), 1001)
        }
      }
    }

    describe ("when the stack register is set to 0x1234") {
      subject.setMemory (0x5E, 0x12)
      subject.setMemory (0x5D, 0x34)

      it ("shows the value as sp") {
        assert (subject.sp === 0x1234)
      }
    }
  }
}
