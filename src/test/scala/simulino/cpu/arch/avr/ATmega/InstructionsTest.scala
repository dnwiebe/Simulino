package simulino.cpu.arch.avr.ATmega

import org.scalatest.path
import simulino.cpu.arch.avr.{AvrCpu, WriteIOSpace}
import simulino.cpu.{SetIp, IncrementIp}
import simulino.cpu.arch.avr.ATmega.Flag._
import simulino.memory.Memory
import simulino.utils.TestUtils._
import org.mockito.Mockito._

/**
 * Created by dnwiebe on 5/12/15.
 */
class InstructionsTest extends path.FunSpec {

  describe ("Given a mock CPU") {
    val cpu = mock (classOf[AvrCpu])

    describe ("ADD") {
      it ("is properly unrecognized") {
        assert (ADD (unsignedBytes (0x08, 0x40)) === None)
      }

      describe ("when properly parsed") {
        val instruction = ADD (unsignedBytes (0xA5, 0x0E)).get

        it ("has the right parameters") {
          assert (instruction.d === 0x0A)
          assert (instruction.r === 0x15)
        }

        it ("is two bytes long") {
          assert (instruction.length === 2)
        }

        it ("takes one clock cycle") {
          assert (instruction.latency === 1)
        }

        describe ("when executed to produce no carry") {
          when (cpu.register (0x0A)).thenReturn (100)
          when (cpu.register (0x15)).thenReturn (23)
          val result = instruction.execute (cpu)

          it ("produces the correct events") {
            assert (result === List (IncrementIp (2), SetRegister (0x0A, 123),
              SetFlags (H = Some (false), S = Some (false), V = Some (false), N = Some (false),
                Z = Some (false), C = Some (false))))
          }
        }

        describe ("when executed to produce a carry") {
          when (cpu.register (0x0A)).thenReturn (200)
          when (cpu.register (0x15)).thenReturn (56)
          val result = instruction.execute (cpu)

          it ("produces the correct events") {
            assert (result === List (IncrementIp (2), SetRegister (0x0A, 0),
              SetFlags (H = Some (true), S = Some (false), V = Some (false), N = Some (false),
                Z = Some (true), C = Some (true))))
          }
        }
      }
    }

    describe ("CP") {
      it ("is properly unrecognized") {
        assert (CP (unsignedBytes (0x1C, 0x01)) === None)
      }

      describe ("when properly parsed") {
        val instruction = CP (unsignedBytes (0xA5, 0x16)).get

        it ("has the proper parameters") {
          assert (instruction.d === 0x0A)
          assert (instruction.r === 0x15)
        }

        it ("is two bytes long") {
          assert (instruction.length === 2)
        }

        it ("takes one clock cycle") {
          assert (instruction.latency === 1)
        }

        describe ("and executed with positive d greater than positive r") {
          when (cpu.register (0x0A)).thenReturn (112)
          when (cpu.register (0x15)).thenReturn (34)
          val result = instruction.execute (cpu)

          it ("produces the correct events") {
            assert (result === List (IncrementIp (2), SetFlags (None, None, Some (true), Some (false), Some (false),
              Some (false), Some (false), Some (false))))
          }
        }

        describe ("and executed with positive d less than positive r") {
          when (cpu.register (0x0A)).thenReturn (12)
          when (cpu.register (0x15)).thenReturn (34)
          val result = instruction.execute (cpu)

          it ("produces the correct events") {
            assert (result === List (IncrementIp (2), SetFlags (None, None, Some (false), Some (true), Some (false),
              Some (true), Some (false), Some (true))))
          }
        }

        describe ("and executed with negative d and positive r") {
          when (cpu.register (0x0A)).thenReturn (250)
          when (cpu.register (0x15)).thenReturn (34)
          val result = instruction.execute (cpu)

          it ("produces the correct events") {
            assert (result === List (IncrementIp (2), SetFlags (None, None, Some (false), Some (true), Some (false),
              Some (true), Some (false), Some (false))))
          }
        }

        describe ("and executed with negative d equal to r") {
          when (cpu.register (0x0A)).thenReturn (250)
          when (cpu.register (0x15)).thenReturn (250)
          val result = instruction.execute (cpu)

          it ("produces the correct events") {
            assert (result === List (IncrementIp (2), SetFlags (None, None, Some (false), Some (false), Some (false),
              Some (false), Some (true), Some (false))))
          }
        }
      }
    }

    describe ("CPC") {
      it ("is properly unrecognized") {
        assert (CPC (unsignedBytes (0x0C, 0x01)) === None)
      }

      describe ("when properly parsed") {
        val instruction = CPC (unsignedBytes (0xA5, 0x06)).get

        it ("has the proper parameters") {
          assert (instruction.d === 0x0A)
          assert (instruction.r === 0x15)
        }

        it ("is two bytes long") {
          assert (instruction.length === 2)
        }

        it ("takes one clock cycle") {
          assert (instruction.latency === 1)
        }

        describe ("and executed with positive d greater than positive r and no carry") {
          when (cpu.register (0x0A)).thenReturn (112)
          when (cpu.register (0x15)).thenReturn (34)
          when (cpu.flag (C)).thenReturn (false)
          val result = instruction.execute (cpu)

          it ("produces the correct events") {
            assert (result === List (IncrementIp (2), SetFlags (None, None, Some (true), Some (false), Some (false),
              Some (false), Some (false), Some (false))))
          }
        }

        describe ("and executed with positive d less than positive r and no carry") {
          when (cpu.register (0x0A)).thenReturn (12)
          when (cpu.register (0x15)).thenReturn (34)
          when (cpu.flag (C)).thenReturn (false)
          val result = instruction.execute (cpu)

          it ("produces the correct events") {
            assert (result === List (IncrementIp (2), SetFlags (None, None, Some (false), Some (true), Some (false),
              Some (true), Some (false), Some (true))))
          }
        }

        describe ("and executed with negative d and positive r and no carry") {
          when (cpu.register (0x0A)).thenReturn (250)
          when (cpu.register (0x15)).thenReturn (34)
          when (cpu.flag (C)).thenReturn (false)
          val result = instruction.execute (cpu)

          it ("produces the correct events") {
            assert (result === List (IncrementIp (2), SetFlags (None, None, Some (false), Some (true), Some (false),
              Some (true), Some (false), Some (false))))
          }
        }

        describe ("and executed with negative d equal to r and no carry") {
          when (cpu.register (0x0A)).thenReturn (250)
          when (cpu.register (0x15)).thenReturn (250)
          when (cpu.flag (C)).thenReturn (false)
          val result = instruction.execute (cpu)

          it ("produces the correct events") {
            assert (result === List (IncrementIp (2), SetFlags (None, None, Some (false), Some (false), Some (false),
              Some (false), None, Some (false))))
          }
        }

        describe ("and executed with positive d greater than positive r and carry") {
          when (cpu.register (0x0A)).thenReturn (112)
          when (cpu.register (0x15)).thenReturn (34)
          when (cpu.flag (C)).thenReturn (true)
          val result = instruction.execute (cpu)

          it ("produces the correct events") {
            assert (result === List (IncrementIp (2), SetFlags (None, None, Some (true), Some (false), Some (false),
              Some (false), Some (false), Some (false))))
          }
        }

        describe ("and executed with positive d less than positive r and carry") {
          when (cpu.register (0x0A)).thenReturn (12)
          when (cpu.register (0x15)).thenReturn (34)
          when (cpu.flag (C)).thenReturn (true)
          val result = instruction.execute (cpu)

          it ("produces the correct events") {
            assert (result === List (IncrementIp (2), SetFlags (None, None, Some (false), Some (true), Some (false),
              Some (true), Some (false), Some (true))))
          }
        }

        describe ("and executed with negative d and positive r and carry") {
          when (cpu.register (0x0A)).thenReturn (250)
          when (cpu.register (0x15)).thenReturn (34)
          when (cpu.flag (C)).thenReturn (true)
          val result = instruction.execute (cpu)

          it ("produces the correct events") {
            assert (result === List (IncrementIp (2), SetFlags (None, None, Some (false), Some (true), Some (false),
              Some (true), Some (false), Some (false))))
          }
        }

        describe ("and executed with negative d equal to r and carry") {
          when (cpu.register (0x0A)).thenReturn (250)
          when (cpu.register (0x15)).thenReturn (250)
          when (cpu.flag (C)).thenReturn (true)
          val result = instruction.execute (cpu)

          it ("produces the correct events") {
            assert (result === List (IncrementIp (2), SetFlags (None, None, Some (true), Some (true), Some (false),
              Some (true), Some (false), Some (true))))
          }
        }
      }
    }

    describe ("CPSE") {
      val programMemory = mock (classOf[Memory])
      when (cpu.ip).thenReturn (1000)
      when (cpu.programMemory).thenReturn (programMemory)

      it ("is properly unrecognized") {
        assert (CPSE (unsignedBytes (0x14, 0x01)) === None)
      }

      describe ("when properly parsed when registers are unequal") {
        when (cpu.register (0x0A)).thenReturn (64)
        when (cpu.register (0x15)).thenReturn (63)
        val instruction = CPSE (unsignedBytes (0xA5, 0x12)).get

        it ("has the proper parameters") {
          assert (instruction.d === 0x0A)
          assert (instruction.r === 0x15)
        }

        it ("is two bytes long") {
          assert (instruction.length === 2)
        }

        describe ("and executed") {
          val result = instruction.execute (cpu)

          it ("produces the correct event") {
            assert (result === List (IncrementIp (2)))
          }

          it ("takes one cycle") {
            assert (instruction.latency === 1)
          }
        }
      }

      describe ("when properly parsed when registers are equal and followed by a two-byte instruction") {
        when (cpu.register (0x0A)).thenReturn (64)
        when (cpu.register (0x15)).thenReturn (64)
        when (programMemory.getData (1002, 2)).thenReturn (unsignedBytes (0xA5, 0x06))
        val instruction = CPSE (unsignedBytes (0xA5, 0x11)).get

        describe ("and executed") {
          val result = instruction.execute (cpu)

          it ("produces the correct event") {
            assert (result === List (IncrementIp (4)))
          }

          it ("takes two cycles") {
            assert (instruction.latency === 2)
          }
        }
      }

      describe ("when properly parsed when registers are equal and followed by a four-byte instruction") {
        when (cpu.register (0x0A)).thenReturn (64)
        when (cpu.register (0x15)).thenReturn (64)
        when (programMemory.getData (1002, 2)).thenReturn (unsignedBytes (0x0C, 0x94))
        val instruction = CPSE (unsignedBytes (0xA5, 0x11)).get

        describe ("and executed") {
          val result = instruction.execute (cpu)

          it ("produces the correct event") {
            assert (result === List (IncrementIp (6)))
          }

          it ("takes three cycles") {
            assert (instruction.latency === 3)
          }
        }
      }
    }

    describe ("EOR") {
      it ("is properly unrecognized") {
        assert (EOR (unsignedBytes (0x28)) === None)
      }

      describe ("when properly parsed with different operands") {
        when (cpu.register (0x0A)).thenReturn (0xAA)
        when (cpu.register (0x15)).thenReturn (0x55)
        val instruction = EOR (unsignedBytes (0xA5, 0x26)).get

        it ("has the right parameters") {
          assert (instruction.d === 0x0A)
          assert (instruction.r === 0x15)
        }

        it ("is two bytes long") {
          assert (instruction.length === 2)
        }

        it ("takes one clock cycle") {
          assert (instruction.latency === 1)
        }

        describe ("and executed") {
          val result = instruction.execute (cpu)

          it ("produces the proper events") {
            assert (result === List (IncrementIp (2), SetRegister (10, 0xFF),
              SetFlags (S = Some (true), V = Some (false), N = Some (true), Z = Some (false))))
          }
        }
      }

      describe ("when properly parsed with equal operands") {
        when (cpu.register (0x0A)).thenReturn (0xAA)
        when (cpu.register (0x15)).thenReturn (0xAA)
        val instruction = EOR (unsignedBytes (0xA5, 0x26)).get

        describe ("and executed") {
          val result = instruction.execute (cpu)

          it ("produces the proper events") {
            assert (result === List (IncrementIp (2), SetRegister (10, 0x00),
              SetFlags (S = Some (false), V = Some (false), N = Some (false), Z = Some (true))))
          }
        }
      }
    }

    describe ("JMP -- note, this instruction is not available in all AVR cores") {
      it ("is properly unrecognized") {
        assert (JMP (unsignedBytes (0xFD, 0x84, 0xFF, 0xFF)) === None)
      }

      describe ("when properly parsed") {
        val instruction = JMP (unsignedBytes (0x5C, 0x95, 0xAA, 0xAA)).get

        it ("is four bytes long") {
          assert (instruction.length === 4)
        }

        it ("takes three clock cycles") {
          assert (instruction.latency === 3)
        }

        describe ("when executed") {
          val result = instruction.execute (cpu)

          it ("generates an event to set the IP") {
            assert (result === List (SetIp (0x555554)))
          }
        }
      }
    }

    describe ("LDI") {
      it ("is properly unrecognized") {
        assert (LDI (unsignedBytes (0xF0, 0x00)) === None)
      }

      describe ("when properly parsed") {
        val instruction = LDI (unsignedBytes (0x5A, 0xEA)).get

        it ("produces the correct parameters") {
          assert (instruction.d === 0x15)
          assert (instruction.K === 0xAA)
        }

        it ("is two bytes long") {
          assert (instruction.length === 2)
        }

        it ("takes one cycle") {
          assert (instruction.latency === 1)
        }

        describe ("when executed") {
          val result = instruction.execute (cpu)

          it ("produces the correct events") {
            assert (result === List (IncrementIp (2), SetRegister (0x15, 0xAA)))
          }
        }
      }
    }

    describe ("MULS") {
      it ("is properly unrecognized") {
        assert (MULS (unsignedBytes (0x00, 0x12)) == None)
      }

      describe ("when properly parsed with two positives") {
        when (cpu.register (2)).thenReturn (78)
        when (cpu.register (3)).thenReturn (87)
        val instruction = MULS (unsignedBytes (0x23, 0x02)).get

        it ("is two bytes long") {
          assert (instruction.length === 2)
        }

        it ("takes two clock cycles") {
          assert (instruction.latency === 2)
        }

        describe ("and executed") {
          val result = instruction.execute (cpu)

          it ("generates the proper events") {
            assert (result === List (IncrementIp (2), SetRegister (1, 0x1A), SetRegister (0, 0x82),
              SetFlags (C = Some (false), Z = Some (false))))
          }
        }
      }

      describe ("when properly parsed with negative and positive") {
        when (cpu.register (2)).thenReturn (-78)
        when (cpu.register (3)).thenReturn (87)
        val instruction = MULS (unsignedBytes (0x23, 0x02)).get

        describe ("and executed") {
          val result = instruction.execute (cpu)

          it ("generates the proper events") {
            assert (result === List (IncrementIp (2), SetRegister (1, 0xE5), SetRegister (0, 0x7E),
              SetFlags (C = Some (true), Z = Some (false))))
          }
        }
      }

      describe ("when properly parsed with a zero") {
        when (cpu.register (2)).thenReturn (0)
        when (cpu.register (3)).thenReturn (87)
        val instruction = MULS (unsignedBytes (0x23, 0x02)).get

        describe ("and executed") {
          val result = instruction.execute (cpu)

          it ("generates the proper events") {
            assert (result === List (IncrementIp (2), SetRegister (1, 0x00), SetRegister (0, 0x00),
              SetFlags (C = Some (false), Z = Some (true))))
          }
        }
      }
    }

    describe ("NOP") {
      it ("is properly unrecognized") {
        assert (NOP (unsignedBytes (0x01, 0x00)) === None)
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

    describe ("OUT") {
      it ("is properly unrecognized") {
        assert (OUT (unsignedBytes (0x1F, 0xAE)) === None)
      }

      describe ("when properly parsed") {
        when (cpu.register (0x15)).thenReturn (0xA5)
        val instruction = OUT (unsignedBytes (0x5A, 0xBB)).get

        it ("has the right parameters") {
          assert (instruction.A === 0x1A)
          assert (instruction.r === 0x15)
        }

        it ("is two bytes long") {
          assert (instruction.length === 2)
        }

        it ("takes one cycle") {
          assert (instruction.latency === 1)
        }

        describe ("when executed") {
          val result = instruction.execute (cpu)

          it ("generates the proper events") {
            assert (result === List (IncrementIp (2), WriteIOSpace (0x1A, 0xA5)))
          }
        }
      }
    }

    describe ("RJMP") {
      it ("is properly unrecognized") {
        assert (RJMP (unsignedBytes (0xD0)) === None)
      }

      describe ("when properly parsed with a positive increment") {
        val instruction = RJMP (unsignedBytes (0x23, 0xC1)).get

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
            assert (result === List(IncrementIp (0x248)))
          }
        }
      }

      describe ("when properly parsed with a negative increment") {
        val instruction = RJMP (unsignedBytes (0xDD, 0xCE)).get

        it ("has the right parameters" ) {
          assert (instruction.k === -291)
        }

        describe ("when executed") {
          val result = instruction.execute (cpu)

          it ("produces an IP increment") {
            assert (result === List(IncrementIp (-580)))
          }
        }
      }
    }

    describe ("SBC") {
      it ("is properly unrecognized") {
        assert (SBC (unsignedBytes (0xC0, 0x00)) === None)
      }

      describe ("when properly parsed") {
        val instruction = SBC (unsignedBytes (0xA5, 0x0A)).get

        it ("has the right parameters") {
          assert (instruction.d === 0x0A)
          assert (instruction.r === 0x15)
        }

        it ("is two bytes long") {
          assert (instruction.length === 2)
        }

        it ("takes one clock cycle") {
          assert (instruction.latency === 1)
        }

        describe ("when executed without a previous carry and producing no carry") {
          when (cpu.flag (C)).thenReturn (false)
          when (cpu.register (0x0A)).thenReturn (123)
          when (cpu.register (0x15)).thenReturn (23)
          val result = instruction.execute (cpu)

          it ("produces the correct events") {
            assert (result === List(IncrementIp (2), SetRegister (0x0A, 100),
              SetFlags (H = Some(false), S = Some (false), V = Some (false), N = Some (false),
                Z = Some (false), C = Some (false))))
          }
        }

        describe ("when executed with a previous carry and producing no carry") {
          when (cpu.flag (C)).thenReturn (true)
          when (cpu.register (0x0A)).thenReturn (123)
          when (cpu.register (0x15)).thenReturn (23)
          val result = instruction.execute (cpu)

          it ("produces the correct events") {
            assert (result === List(IncrementIp (2), SetRegister (0x0A, 99),
              SetFlags (H = Some(false), S = Some (false), V = Some (false), N = Some (false),
                Z = Some (false), C = Some (false))))
          }
        }

        describe ("when executed without a previous carry and producing a carry") {
          when (cpu.flag (C)).thenReturn (false)
          when (cpu.register (0x0A)).thenReturn (23)
          when (cpu.register (0x15)).thenReturn (123)
          val result = instruction.execute (cpu)

          it ("produces the correct events") {
            assert (result === List(IncrementIp (2), SetRegister (0x0A, 156),
              SetFlags (H = Some(true), S = Some (true), V = Some (false), N = Some (true),
                Z = Some (false), C = Some (true))))
          }
        }

        describe ("when executed with a previous carry and producing a carry") {
          when (cpu.flag (C)).thenReturn (true)
          when (cpu.register (0x0A)).thenReturn (23)
          when (cpu.register (0x15)).thenReturn (123)
          val result = instruction.execute (cpu)

          it ("produces the correct events") {
            assert (result === List(IncrementIp (2), SetRegister (0x0A, 155),
              SetFlags (H = Some(true), S = Some (true), V = Some (false), N = Some (true),
                Z = Some (false), C = Some (true))))
          }
        }
      }
    }
  }
}
