package simulino.cpu.arch

import org.mockito.Matchers
import simulino.cpu.arch.avr.ATmega.{ADD, SetMemory, SetFlags, Flag}
import simulino.cpu.arch.avr.ATmega.Flag._

import org.scalatest.path
import org.mockito.Mockito._
import simulino.cpu._
import simulino.cpu.arch.avr.AvrCpu
import simulino.cpu.arch.avr.RegisterNames._
import simulino.engine.{Event, Engine}
import simulino.memory.{Span, UnsignedByte, Memory}
import simulino.simulator.SimulatorConfiguration
import simulino.utils.TestUtils._

import scala.util.Try

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

    it ("has 8704 bytes of memory (8192 SRAM + 512 internal)") {
      subject.dataMemory.getData (0x21FF, 1) // no exception
      val result = Try {subject.dataMemory.getData (0x2200, 1)}
      fails (result, new IllegalArgumentException ("Data buffer must end at or before 8703, not 8704"))
    }

    it ("initially has zeros in all registers") {
      (0x00 until 0x200).foreach {i => assert (subject.register (i) === UnsignedByte (0), s"Register ${i}")}
    }

    it ("initially has zeros in all flags") {
      Flag.values ().foreach { f =>
        assert (subject.flag (f) === false, s"Flag ${f}")
      }
    }

    it ("starts with 0 in the SP") {
      assert (subject.sp === 0)
    }

    describe ("directed to set SP") {
      subject.receive (SetSp (1000))

      it ("does so") {
        assert (subject.sp === 1000)
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

    describe ("directed to push a constant") {
      subject.setSpForTest (500)
      subject.receive (Push (42))

      it ("stores the constant on the stack") {
        assert (subject.dataMemory.getData (500, 1)(0) === UnsignedByte (42))
      }

      it ("decrements the stack pointer") {
        assert (subject.sp === 499)
      }
    }

    describe ("directed to pop to a location") {
      subject.setSpForTest (500)
      subject.dataMemory.update (501, 42)
      subject.receive (Pop (0x15))

      it ("retrieves the data properly") {
        assert (subject.dataMemory.getData (0x15, 1)(0) === UnsignedByte (42))
      }

      it ("leaves the stack pointer incremented") {
        assert (subject.sp === 501)
      }
    }

    describe ("directed to push the Instruction Pointer") {
      subject.setIpForTest (0x123456)
      subject.setSpForTest (500)
      subject.receive (PushIp ())

      it ("stores the IP on the stack") {
        assert (subject.dataMemory.getData (498, 3) === unsignedBytes (0x12, 0x34, 0x58))
      }

      it ("reduces the stack pointer") {
        assert (subject.sp === 497)
      }
    }

    describe ("directed to pop the Instruction Pointer") {
      subject.setSpForTest (500)
      subject.dataMemory.addSpan (Span (501, unsignedBytes (0x12, 0x34, 0x56)))
      subject.receive (PopIp ())

      it ("sets IP properly") {
        assert (subject.ip === 0x123456)
      }

      it ("leaves the stack pointer incremented") {
        assert (subject.sp === 503)
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
      subject.setMemory (SPH, 0x12)
      subject.setMemory (SPL, 0x34)

      it ("shows the value as sp") {
        assert (subject.sp === 0x1234)
      }
    }

    describe ("when the global interrupt flag is clear") {
      subject.dataMemory.update (SREG, 0x00)

      describe ("and an interrupt is raised") {
        subject.raiseInterrupt ("TIM0_OVF")

        it ("nothing is scheduled for the engine") {
          verify (engine, never).schedule (Matchers.any (classOf[Event]), Matchers.anyLong ())
        }
      }
    }

    describe ("when the global interrupt flag is set") {
      subject.dataMemory.update (SREG, 0x80)

      describe ("and some interrupts are raised") {
        subject.raiseInterrupt ("USART3_TXC") // 0xE0
        subject.raiseInterrupt ("TIM0_OVF") // 0x5C
        subject.raiseInterrupt ("SPM_RDY") // 0xA0

        it ("those interrupts' vectors are active") {
          assert (subject.activeInterrupts === Set (0xE0, 0xA0, 0x5C))
        }

        describe ("and a ScheduleNextInstruction is received") {
          when (engine.currentTick).thenReturn (4096L)
          subject.setIpForTest (1000)
          subject.receive (ScheduleNextInstruction ())

          it ("the highest-priority active interrupt is gone") {
            assert (subject.activeInterrupts === Set (0xE0, 0xA0))
          }

          it ("the correct events are scheduled for the engine") {
            val order = inOrder (engine)
            order.verify (engine).schedule (PushIp (), 4101L)
            order.verify (engine).schedule (SetIp (0x5C), 4101L)
            order.verify (engine).schedule (SetFlags (I = Some (false)), 4101L)
            order.verify (engine).schedule (ScheduleNextInstruction (), 4101L)
          }
        }
      }
    }
  }
}
