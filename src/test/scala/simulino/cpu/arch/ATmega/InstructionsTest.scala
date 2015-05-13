package simulino.cpu.arch.ATmega

import org.scalatest.path
import simulino.cpu.arch.AvrCpu
import simulino.cpu.{IncrementIp, Cpu}
import simulino.utils.TestUtils._
import org.mockito.Mockito._

/**
 * Created by dnwiebe on 5/12/15.
 */
class InstructionsTest extends path.FunSpec {

  describe ("Given a mock CPU") {
    val cpu = mock (classOf[AvrCpu])

    describe ("NOP") {
      it ("is properly unrecognized") {
        assert (NOP (unsignedBytes (0x00, 0x01)) === None)
      }

      describe ("when properly parsed") {
        val instruction = NOP (unsignedBytes (0x00, 0x00)).get

        it ("is two bytes long") {
          assert (instruction.length === 2)
        }

        it ("takes one clock cycle") {
          assert (instruction.latency === 1)
        }

        describe ("when executed") {
          val result = instruction.execute (cpu)

          it ("produces an IP increment") {
            assert (result === List (IncrementIp (2)))
          }
        }
      }
    }

    describe ("RJMP") {
      it ("is properly unrecognized") {
        assert (RJMP (unsignedBytes (0xD0)) === None)
      }

      describe ("when properly parsed") {
        val instruction = RJMP (unsignedBytes (0xC1, 0x23)).get

        it ("has the right parameters" ) {
          assert (instruction.k === 0x123)
        }

        it ("is two bytes long") {
          assert (instruction.length === 2)
        }

        it ("takes two clock cycles") {
          assert (instruction.latency === 2)
        }

        describe ("when executed") {
          val result = instruction.execute (cpu)

          it ("produces an IP increment") {
            assert (result === List(IncrementIp (0x246)))
          }
        }
      }
    }

    describe ("SBC") {
      it ("is properly unrecognized") {
        assert (SBC (unsignedBytes (0x00, 0xC0)) === None)
      }

      describe ("when properly parsed") {
        val instruction = SBC (unsignedBytes (0x0A, 0xA5)).get

        it ("has the right parameters") {
          assert (instruction.r === 0x15)
          assert (instruction.d === 0x0A)
        }

        it ("is two bytes long") {
          assert (instruction.length === 2)
        }

        it ("takes one clock cycle") {
          assert (instruction.latency === 1)
        }

        describe ("when executed without a previous carry and producing no carry") {
          when (cpu.flag ('C)).thenReturn (false)
          when (cpu.register (0x15)).thenReturn (23)
          when (cpu.register (0x0A)).thenReturn (123)
          val result = instruction.execute (cpu)

          it ("produces the correct instructions") {
            assert (result === List(IncrementIp (2), SetRegister (0x0A, 100),
              SetFlags (H = Some(false), S = Some (true), V = Some (false), N = Some (false),
              Z = Some (false), C = Some (false))))
          }
        }
      }
    }
  }
}
