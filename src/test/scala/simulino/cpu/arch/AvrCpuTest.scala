package simulino.cpu.arch

import simulino.cpu.arch.avr.ATmega.{ADD, SetRegister, SetFlags, Flag}
import simulino.cpu.arch.avr.ATmega.Flag._

import org.scalatest.path
import org.mockito.Mockito._
import simulino.cpu.IncrementIp
import simulino.cpu.arch.avr.{RegisterFile, AvrCpu}
import simulino.engine.Engine
import simulino.memory.{UnsignedByte, Span, Memory}
import simulino.simulator.CpuConfiguration
import simulino.simulator.peripheral.PinSampler
import simulino.utils.TestUtils._

/**
 * Created by dnwiebe on 5/14/15.
 */
class AvrCpuTest extends path.FunSpec {
  describe ("An AvrCpu with a mock Engine, Memory, and RegisterFile") {
    val engine = mock (classOf[Engine])
    when (engine.currentTick).thenReturn (1000)
    val memory = mock (classOf[Memory])
    val config = mock (classOf[CpuConfiguration])

    val subject = new AvrCpu (engine, memory, config)

    it ("initially has zeros in all registers") {
      (0x00 until 0x200).foreach {i => assert (subject.register (i) === UnsignedByte (0), s"Register ${i}")}
    }

    it ("initially has zeros in all flags") {
      Flag.values ().foreach { f =>
        assert (subject.flag (f) === false, s"Flag ${f}")
      }
    }

    describe ("directed to set R28 to 47") {
      subject.receive (SetRegister (28, 47))

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

        describe ("and then to clear the rest") {
          subject.receive (SetFlags (T = Some (false), S = Some (false), N = Some (false), C = Some (false)))

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

    describe ("when set up with ones in Registers 0 and 1") {
      subject.setRegister(0, UnsignedByte (1))
      subject.setRegister(1, UnsignedByte (1))

      describe ("and instructed to add the contents of R1 to the contents of R0") {
        subject.receive (new ADD (0, 1))

        describe ("schedules the correct events for the correct clock tick") {
          verify (engine).schedule (IncrementIp (2), 1001)
          verify (engine).schedule (SetRegister (0, 2), 1001)
          verify (engine).schedule (SetFlags (None, None, Some (false), Some (false), Some (false), Some (false),
            Some (false), Some (false)), 1001)
        }
      }
    }

    describe ("when the stack register is set to 0x1234") {
      subject.setRegister (0x5E, 0x12)
      subject.setRegister (0x5D, 0x34)

      it ("shows the value as sp") {
        assert (subject.sp === 0x1234)
      }
    }
  }
}
