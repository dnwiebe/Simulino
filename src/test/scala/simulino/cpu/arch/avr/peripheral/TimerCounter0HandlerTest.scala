package simulino.cpu.arch.avr.peripheral

import org.mockito.Matchers
import org.scalatest.path
import simulino.cpu.arch.avr.{AvrCpu, PortMap}
import org.mockito.Mockito._

/**
 * Created by dnwiebe on 5/25/15.
 */
class TimerCounter0HandlerTest extends path.FunSpec {
  describe ("A TimerCounter0Handler") {
    val cpu = mock (classOf[AvrCpu])
    val portMap = mock (classOf[PortMap])
    when (cpu.portMap).thenReturn (portMap)
    val subject = new TimerCounter0Handler ()
    subject.initialize (cpu)

    describe ("when a 1 is written to TOV0") {
      subject.acceptChange ("TOV0", 0, 1)

      it ("clears the flag") {
        verify (portMap).writeToPort ("TOV0", 0)
      }
    }

    describe ("with all three bits of WGM0 used") {
      when (portMap.readFromPort ("WGM02")).thenReturn (1)
      when (portMap.readFromPort ("WGM0")).thenReturn (1)

      describe ("when the WGM method is called") {
        val result = subject.WGM

        it ("the result is cobbled together properly") {
          assert (result === 5)
        }
      }
    }

    describe ("in Normal Mode, clocking from the system clock") {
      when (portMap.readFromPort ("WGM02")).thenReturn (0)
      when (portMap.readFromPort ("WGM0")).thenReturn (0)
      when (portMap.readFromPort ("CS0")).thenReturn (1)

      describe ("in the middle of the range") {
        when (portMap.readFromPort ("TCNT0")).thenReturn (0x95)

        describe ("a tick is received") {
          subject.tick (1L)

          it ("increments the counter") {
            verify (portMap).writeToPort ("TCNT0", 0x96)
          }

          it ("does not strobe the overflow flag") {
            verify (portMap, never).writeToPort (Matchers.eq ("TOV0"), Matchers.anyInt ())
          }
        }
      }

      describe ("at the top of the range") {
        when (portMap.readFromPort ("TCNT0")).thenReturn (0xFF)

        describe ("a tick is received") {
          subject.tick (1L)

          it ("resets the counter") {
            verify (portMap).writeToPort ("TCNT0", 0x00)
          }

          it ("strobes the overflow flag") {
            verify (portMap).writeToPort ("TOV0", 1)
          }

          it ("does not raise an interrupt") {
            verify (cpu, never).raiseInterrupt (Matchers.anyString ())
          }
        }
      }
    }

    describe ("in Clear Timer On Compare Match mode, clocking from the system clock") {
      when (portMap.readFromPort ("WGM02")).thenReturn (0)
      when (portMap.readFromPort ("WGM0")).thenReturn (2)
      when (portMap.readFromPort ("CS0")).thenReturn (1)

      describe ("in the middle of the range") {
        when (portMap.readFromPort ("OCR0A")).thenReturn (0x80)
        when (portMap.readFromPort ("TCNT0")).thenReturn (0x45)

        describe ("a tick is received") {
          subject.tick (1L)

          it ("increments the counter") {
            verify (portMap).writeToPort ("TCNT0", 0x46)
          }

          it ("does not strobe the overflow flag") {
            verify (portMap, never).writeToPort (Matchers.eq ("TOV0"), Matchers.anyInt ())
          }
        }
      }

      describe ("at the top of the range") {
        when (portMap.readFromPort ("OCR0A")).thenReturn (0x80)
        when (portMap.readFromPort ("TCNT0")).thenReturn (0x80)

        describe ("a tick is received") {
          subject.tick (1L)

          it ("resets the counter") {
            verify (portMap).writeToPort ("TCNT0", 0x00)
          }

          it ("strobes the overflow flag") {
            verify (portMap).writeToPort ("TOV0", 1)
          }
        }
      }

      describe ("when OCR0A is zero") {
        when (portMap.readFromPort ("OCR0A")).thenReturn (0x00)
        when (portMap.readFromPort ("TCNT0")).thenReturn (0x00).thenReturn (0x01).thenReturn (0x00)

        describe ("and a tick is received") {
          subject.tick (1L)

          it ("resets the counter") {
            verify (portMap).writeToPort ("TCNT0", 0x00)
          }

          it ("strobes the overflow flag") {
            verify (portMap).writeToPort ("TOV0", 1)
          }
        }
      }
    }

    describe ("in Fast PWM Mode") {
      when (portMap.readFromPort ("WGM0")).thenReturn (3)
      describe ("and OCR0A is 0x80") {
        when (portMap.readFromPort ("OCR0A")).thenReturn (0x80)
        describe ("and WGM02 is clear") {
          when (portMap.readFromPort ("WGM02")).thenReturn (0)
          describe ("and TCNT0 is 0x80") {
            when (portMap.readFromPort ("TCNT0")).thenReturn (0x80)
            describe ("and a tick is received") {
              subject.tick (1L)

              it ("keeps counting") {
                verify (portMap).writeToPort ("TCNT0", 0x81)
              }

              it ("does not strobe the overflow flag") {
                verify (portMap, never).writeToPort (Matchers.eq ("TOV0"), Matchers.anyInt ())
              }
            }
          }
          describe ("and TCNT0 is 0xFF") {
            when (portMap.readFromPort ("TCNT0")).thenReturn (0xFF)
            describe ("and a tick is received") {
              subject.tick (1L)

              it ("resets the counter") {
                verify (portMap).writeToPort ("TCNT0", 0x00)
              }

              it ("strobes the overflow flag") {
                verify (portMap).writeToPort ("TOV0", 1)
              }
            }
          }
        }
      }
    }
  }
}
