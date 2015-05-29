package simulino.cpu.arch.avr.ATmega

import org.scalatest.path
import simulino.cpu.arch.avr.AvrCpu
import simulino.cpu.{PushIp, SetIp, IncrementIp}
import simulino.cpu.arch.avr.ATmega.Flag._
import simulino.cpu.arch.avr.ATmega.IndirectionType._
import simulino.cpu.arch.avr.RegisterNames._
import simulino.memory.{UnsignedByte, Memory}
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
            assert (result === List (IncrementIp (2), SetMemory (0x0A, 123),
              SetFlags (H = Some (false), S = Some (false), V = Some (false), N = Some (false),
                Z = Some (false), C = Some (false))))
          }
        }

        describe ("when executed to produce a carry") {
          when (cpu.register (0x0A)).thenReturn (200)
          when (cpu.register (0x15)).thenReturn (56)
          val result = instruction.execute (cpu)

          it ("produces the correct events") {
            assert (result === List (IncrementIp (2), SetMemory (0x0A, 0),
              SetFlags (H = Some (true), S = Some (false), V = Some (false), N = Some (false),
                Z = Some (true), C = Some (true))))
          }
        }
      }
    }

    describe ("BRBC") {
      it ("is properly unrecognized") {
        assert (BRBC (unsignedBytes (0x77, 0xE4)) === None)
      }

      describe ("when set to branch on bit Z") {
        val instruction = BRBC (unsignedBytes (0x59, 0xF7)).get

        it ("has the right parameters") {
          assert (instruction.s === 1)
          assert (instruction.k === -21)
        }

        it ("is two bytes long") {
          assert (instruction.length === 2)
        }

        describe ("when executed with bit Z clear") {
          when (cpu.register (SREG)).thenReturn (UnsignedByte (0x00))
          val result = instruction.execute (cpu)

          it ("takes two cycles") {
            assert (instruction.latency === 2)
          }

          it ("creates the proper events") {
            assert (result === List (IncrementIp (-40)))
          }
        }

        describe ("when executed with bit Z set") {
          when (cpu.register (SREG)).thenReturn (UnsignedByte (0x02))
          val result = instruction.execute (cpu)

          it ("takes one cycle") {
            assert (instruction.latency === 1)
          }

          it ("creates the proper events") {
            assert (result === List (IncrementIp (2)))
          }
        }
      }

      describe ("when set to branch on bit V") {
        val instruction = BRBC (unsignedBytes (0xAB, 0xF4)).get

        it ("has the right parameters") {
          assert (instruction.s === 3)
          assert (instruction.k === 21)
        }

        describe ("when executed with bit V clear") {
          when (cpu.register (SREG)).thenReturn (UnsignedByte (0x00))
          val result = instruction.execute (cpu)

          it ("takes two cycles") {
            assert (instruction.latency === 2)
          }

          it ("creates the proper events") {
            assert (result === List (IncrementIp (44)))
          }
        }

        describe ("when executed with bit V set") {
          when (cpu.register (SREG)).thenReturn (UnsignedByte (0x08))
          val result = instruction.execute (cpu)

          it ("takes one cycle") {
            assert (instruction.latency === 1)
          }

          it ("creates the proper events") {
            assert (result === List (IncrementIp (2)))
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

    describe ("CPI") {
      it ("is properly unrecognized") {
        assert (CPI (unsignedBytes (0x67, 0x74)) === None)
      }

      describe ("when comparing equals") {
        when (cpu.register (0x15)).thenReturn (0x93)
        val instruction = CPI (unsignedBytes (0x53, 0x39)).get

        it ("has the right parameters") {
          assert (instruction.d === 0x15)
          assert (instruction.K === UnsignedByte (0x93))
        }

        it ("is two bytes long") {
          assert (instruction.length === 2)
        }

        it ("takes one cycle") {
          assert (instruction.latency === 1)
        }

        describe ("and executed") {
          val result = instruction.execute (cpu)

          it ("produces the correct events") {
            assert (result === List (IncrementIp (2),
              SetFlags (H = Some (false), S = Some (false), V = Some (false), N = Some (false), Z = Some (true), C = Some (false))))
          }
        }
      }

      describe ("when comparing large to small") {
        when (cpu.register (0x15)).thenReturn (0x73)
        val instruction = CPI (unsignedBytes (0x53, 0x39)).get

        describe ("and executed") {
          val result = instruction.execute (cpu)

          it ("produces the correct events") {
            assert (result === List (IncrementIp (2),
              SetFlags (H = Some (false), S = Some (false), V = Some (true), N = Some (true), Z = Some (false), C = Some (true))))
          }
        }
      }

      describe ("when comparing small to large") {
        when (cpu.register (0x15)).thenReturn (0x93)
        val instruction = CPI (unsignedBytes (0x53, 0x37)).get

        describe ("and executed") {
          val result = instruction.execute (cpu)

          it ("produces the correct events") {
            assert (result === List (IncrementIp (2),
              SetFlags (H = Some (false), S = Some (true), V = Some (true), N = Some (false), Z = Some (false), C = Some (false))))
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
            assert (result === List (IncrementIp (2), SetMemory (10, 0xFF),
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
            assert (result === List (IncrementIp (2), SetMemory (10, 0x00),
              SetFlags (S = Some (false), V = Some (false), N = Some (false), Z = Some (true))))
          }
        }
      }
    }

    describe ("IN") {
      it ("is properly unrecognized") {
        assert (IN (unsignedBytes (0x1F, 0xA3)) === None)
      }

      describe ("when properly parsed") {
        when (cpu.register (0x3A)).thenReturn (0xA5)
        val instruction = IN (unsignedBytes (0x5A, 0xB3)).get

        it ("has the right parameters") {
          assert (instruction.A === 0x1A)
          assert (instruction.d === 0x15)
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
            assert (result === List (IncrementIp (2), SetMemory (0x15, 0xA5)))
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

    describe ("LDD") {
      when (cpu.register (ZL)).thenReturn (0x34)
      when (cpu.register (ZH)).thenReturn (0x12)
      when (cpu.register (RAMPZ)).thenReturn (0x00)
      when (cpu.register (0x1234)).thenReturn (42)
      when (cpu.register (0x1233)).thenReturn (41)
      when (cpu.register (0x125E)).thenReturn (43)
      it ("is properly unrecognized") {
        assert (LDD (unsignedBytes (0xF8, 0x80)) === None)
      }

      describe ("case i/iv") {
        describe ("when properly parsed with ds and no qs") {
          val instruction = LDD (unsignedBytes (0xF0, 0x81)).get

          it ("produces the correct parameters") {
            assert (instruction.d === 0x1F)
            assert (instruction.x === IndirectionType.Unchanged)
            assert (instruction.q === 0x0)
          }

          it ("takes one cycle") {
            assert (instruction.latency === 1)
          }
        }
        describe ("when properly parsed with qs and no ds") {
          val instruction = LDD (unsignedBytes (0x07, 0xAC)).get

          it ("produces the correct parameters") {
            assert (instruction.d === 0x0)
            assert (instruction.x === IndirectionType.Unchanged)
            assert (instruction.q === 0x3F)
          }
        }
        describe ("with qs and ds") {
          val instruction = new LDD (0x15, IndirectionType.Unchanged, 0x2A)

          it ("is two bytes long") {
            assert (instruction.length === 2)
          }

          it ("takes two cycles") {
            assert (instruction.latency === 2)
          }

          describe ("when executed") {
            val result = instruction.execute (cpu)

            it ("produces the right events") {
              assert (result === List (IncrementIp (2), SetMemory (0x15, 43)))
            }
          }
        }
      }

      describe ("case ii") {
        describe ("when properly parsed") {
          val instruction = LDD (unsignedBytes (0x51, 0x91)).get

          it ("produces the correct parameters") {
            assert (instruction.d === 0x15)
            assert (instruction.x === IndirectionType.PostIncrement)
            assert (instruction.q === 0x00)
          }

          it ("is two bytes long") {
            assert (instruction.length === 2)
          }

          it ("takes two cycles") {
            assert (instruction.latency === 2)
          }

          describe ("when executed") {
            val result = instruction.execute (cpu)

            it ("produces the right events") {
              assert (result === List (IncrementIp (2), SetMemory (0x15, 42),
                SetMemory (RAMPZ, 0x00), SetMemory (ZH, 0x12), SetMemory (ZL, 0x35)))
            }
          }
        }
      }

      describe ("case iii") {
        describe ("when properly parsed") {
          val instruction = LDD (unsignedBytes (0x52, 0x91)).get

          it ("produces the correct parameters") {
            assert (instruction.d === 0x15)
            assert (instruction.x === IndirectionType.PreDecrement)
            assert (instruction.q === 0x00)
          }

          it ("is two bytes long") {
            assert (instruction.length === 2)
          }

          it ("takes three cycles") {
            assert (instruction.latency === 3)
          }

          describe ("when executed") {
            val result = instruction.execute (cpu)

            it ("produces the right events") {
              assert (result === List (IncrementIp (2), SetMemory (0x15, 41),
                SetMemory (RAMPZ, 0x00), SetMemory (ZH, 0x12), SetMemory (ZL, 0x33)))
            }
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
            assert (result === List (IncrementIp (2), SetMemory (0x15, 0xAA)))
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
            assert (result === List (IncrementIp (2), SetMemory (1, 0x1A), SetMemory (0, 0x82),
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
            assert (result === List (IncrementIp (2), SetMemory (1, 0xE5), SetMemory (0, 0x7E),
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
            assert (result === List (IncrementIp (2), SetMemory (1, 0x00), SetMemory (0, 0x00),
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

    describe ("ORI") {
      when (cpu.register (0x15)).thenReturn (0x00)
      it ("is properly unrecognized") {
        assert (ORI (unsignedBytes (0x00, 0x07)) === None)
      }

      describe ("when given a nonzero parameter") {
        val instruction = ORI (unsignedBytes (0x54, 0x6B)).get

        it ("has the correct parameters") {
          assert (instruction.d === 0x15)
          assert (instruction.K === 0xB4)
        }

        it ("is two bytes long") {
          assert (instruction.length === 2)
        }

        it ("takes one cycle") {
          assert (instruction.latency === 1)
        }

        describe ("when executed") {
          val result = instruction.execute (cpu)

          it ("generates the correct events") {
            assert (result === List (IncrementIp (2), SetMemory (0x15, 0xB4),
              SetFlags (S = Some (true), V = Some (false), N = Some (true), Z = Some (false))))
          }
        }
      }

      describe ("when given two zero parameters") {
        val instruction = ORI (unsignedBytes (0x50, 0x60)).get

        it ("has the correct parameters") {
          assert (instruction.d === 0x15)
          assert (instruction.K === 0x00)
        }

        describe ("when executed") {
          val result = instruction.execute (cpu)

          it ("generates the correct events") {
            assert (result === List (IncrementIp (2), SetMemory (0x15, 0x00),
              SetFlags (S = Some (false), V = Some (false), N = Some (false), Z = Some (true))))
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
            assert (result === List (IncrementIp (2), SetMemory (0x3A, 0xA5)))
          }
        }
      }
    }

    describe ("RCALL") {
      it ("is properly unrecognized") {
        assert (RCALL (unsignedBytes (0x00, 0xC0)) === None)
      }

      describe ("when properly parsed") {
        val instruction = RCALL (unsignedBytes (0xED, 0xDF)).get

        it ("has the proper parameter") {
          assert (instruction.k === -19)
        }

        it ("is two bytes long") {
          assert (instruction.length === 2)
        }

        it ("takes four cycles") {
          assert (instruction.latency === 4)
        }

        describe ("when executed") {
          val result = instruction.execute (cpu)

          it ("produces the correct results") {
            assert (result === List (PushIp (), IncrementIp (-36)))
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
            assert (result === List(IncrementIp (2), SetMemory (0x0A, 100),
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
            assert (result === List(IncrementIp (2), SetMemory (0x0A, 99),
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
            assert (result === List(IncrementIp (2), SetMemory (0x0A, 156),
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
            assert (result === List(IncrementIp (2), SetMemory (0x0A, 155),
              SetFlags (H = Some(true), S = Some (true), V = Some (false), N = Some (true),
                Z = Some (false), C = Some (true))))
          }
        }
      }
    }

    describe ("SEx") {
      it ("is properly unrecognized") {
        assert (SEx (unsignedBytes (0x78, 0x84)) === None)
      }

      describe ("SEI") {
        val instruction = SEx (unsignedBytes (0x78, 0x94)).get

        it ("is two bytes long") {
          assert (instruction.length === 2)
        }

        it ("takes one cycle") {
          assert (instruction.latency === 1)
        }

        describe ("when executed") {
          val result = instruction.execute (cpu)

          it ("produces the right events") {
            assert (result === List (IncrementIp (2), SetFlags (0x80, 0xFF)))
          }
        }
      }
    }

    describe ("ST") {
      when (cpu.register (0x0A)).thenReturn (UnsignedByte (0x5A))
      when (cpu.register (XL)).thenReturn (0x56)
      when (cpu.register (XH)).thenReturn (0x34)
      when (cpu.register (RAMPX)).thenReturn (0x12)

      it ("is properly unrecognized") {
        assert (ST (unsignedBytes (0x0C, 0x96)) === None)
      }

      it ("won't result from a PUSH") {
        assert (ST (unsignedBytes (0x0F, 0x92)) === None)
      }

      describe ("when properly parsed as Unchanged") {
        val instruction = ST (unsignedBytes (0xAC, 0x92)).get

        it ("has the proper parameters") {
          assert (instruction.x === Unchanged)
          assert (instruction.r === 0x0A)
        }

        it ("is two bytes long") {
          assert (instruction.length === 2)
        }

        it ("takes two cycles") {
          assert (instruction.latency === 2)
        }

        describe ("and executed") {
          val result = instruction.execute (cpu)

          it ("creates the right events") {
            assert (result === List (IncrementIp (2), SetMemory (0x123456, 0x5A),
              SetMemory (RAMPX, 0x12), SetMemory (XH, 0x34), SetMemory (XL, 0x56)))
          }
        }
      }

      describe ("when properly parsed as PostIncrement") {
        val instruction = ST (unsignedBytes (0xAD, 0x92)).get

        it ("has the proper parameters") {
          assert (instruction.x === PostIncrement)
          assert (instruction.r === 0x0A)
        }

        describe ("and executed") {
          val result = instruction.execute (cpu)

          it ("creates the right events") {
            assert (result === List (IncrementIp (2), SetMemory (0x123456, 0x5A),
              SetMemory (RAMPX, 0x12), SetMemory (XH, 0x34), SetMemory (XL, 0x57)))
          }
        }
      }

      describe ("when properly parsed as PreDecrement") {
        val instruction = ST (unsignedBytes (0xAE, 0x92)).get

        it ("has the proper parameters") {
          assert (instruction.x === PreDecrement)
          assert (instruction.r === 0x0A)
        }

        describe ("and executed") {
          val result = instruction.execute (cpu)

          it ("creates the right events") {
            assert (result === List (IncrementIp (2), SetMemory (0x123455, 0x5A),
              SetMemory (RAMPX, 0x12), SetMemory (XH, 0x34), SetMemory (XL, 0x55)))
          }
        }
      }
    }

    describe ("STD") {
      when (cpu.register (0x15)).thenReturn (42)
      when (cpu.register (ZL)).thenReturn (0x34)
      when (cpu.register (ZH)).thenReturn (0x12)
      when (cpu.register (RAMPZ)).thenReturn (0x00)
      it ("is properly unrecognized") {
        assert (STD (unsignedBytes (0xF8, 0x80)) === None)
      }

      describe ("case i/iv") {
        describe ("when properly parsed with ds and no qs") {
          val instruction = STD (unsignedBytes (0xF0, 0x83)).get

          it ("produces the correct parameters") {
            assert (instruction.r === 0x1F)
            assert (instruction.x === IndirectionType.Unchanged)
            assert (instruction.q === 0x0)
          }

          it ("is two bytes long") {
            assert (instruction.length === 2)
          }

          it ("takes two cycles") {
            assert (instruction.latency === 2)
          }
        }
        describe ("when properly parsed with qs and no ds") {
          val instruction = STD (unsignedBytes (0x07, 0xAE)).get

          it ("produces the correct parameters") {
            assert (instruction.r === 0x0)
            assert (instruction.x === IndirectionType.Unchanged)
            assert (instruction.q === 0x3F)
          }
        }
        describe ("with qs and ds") {
          val instruction = new STD (0x15, IndirectionType.Unchanged, 0x2A)

          describe ("when executed") {
            val result = instruction.execute (cpu)

            it ("produces the right events") {
              assert (result === List (IncrementIp (2), SetMemory (0x125E, UnsignedByte (42))))
            }
          }
        }
      }

      describe ("case ii") {
        describe ("when properly parsed") {
          val instruction = STD (unsignedBytes (0x51, 0x93)).get

          it ("produces the correct parameters") {
            assert (instruction.r === 0x15)
            assert (instruction.x === IndirectionType.PostIncrement)
            assert (instruction.q === 0x00)
          }

          describe ("when executed") {
            val result = instruction.execute (cpu)

            it ("produces the right events") {
              assert (result === List (IncrementIp (2), SetMemory (0x1234, 42)) ++
                setExtended (Zfull, 0x001235))
            }
          }
        }
      }

      describe ("case iii") {
        describe ("when properly parsed") {
          val instruction = STD (unsignedBytes (0x52, 0x93)).get

          it ("produces the correct parameters") {
            assert (instruction.r === 0x15)
            assert (instruction.x === IndirectionType.PreDecrement)
            assert (instruction.q === 0x00)
          }

          describe ("when executed") {
            val result = instruction.execute (cpu)

            it ("produces the right events") {
              assert (result === List (IncrementIp (2), SetMemory (0x1233, 42)) ++
                setExtended (Zfull, 0x1233))
            }
          }
        }
      }
    }
  }
}
