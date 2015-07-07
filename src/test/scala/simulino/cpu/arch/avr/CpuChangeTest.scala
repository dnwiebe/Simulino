package simulino.cpu.arch.avr

import org.scalatest.path
import org.mockito.Mockito._
import simulino.cpu.arch.avr.RegisterNames._

/**
 * Created by dnwiebe on 6/7/15.
 */
class CpuChangeTest extends path.FunSpec {
  describe ("Given a mock AvrCpu") {
    val cpu = mock (classOf[AvrCpu])

    describe ("a PushIp") {
      val subject = PushIp ()

      describe ("directed to show mods") {

        when (cpu.ip).thenReturn (0x123456)
        when (cpu.sp).thenReturn (0x21FF)
        when (cpu.getMemory (0x21FF)).thenReturn (0x65)
        when (cpu.getMemory (0x21FE)).thenReturn (0x43)
        when (cpu.getMemory (0x21FD)).thenReturn (0x21)
        val result = subject.mods (cpu)

        it ("does so appropriately") {
          assert (result === "($21FF): $21 -> $56; ($21FE): $43 -> $34; ($21FD): $65 -> $12; SP: $21FF -> $21FC")
        }
      }
    }

    describe ("a Push") {
      val subject = Push (0x42)

      describe ("directed to show mods") {
        when (cpu.sp).thenReturn (0x21FF)
        when (cpu.getMemory (0x21FF)).thenReturn (0x12)
        val result = subject.mods (cpu)

        it ("does so appropriately") {
          assert (result === "($21FF): $12 -> $42; SP: $21FF -> $21FE")
        }
      }
    }

    describe ("a PopIp") {
      val subject = PopIp ()

      describe ("directed to show mods") {
        when (cpu.ip).thenReturn (0x1000)
        when (cpu.sp).thenReturn (0x21FC)
        when (cpu.getMemory (0x21FD)).thenReturn (0x12)
        when (cpu.getMemory (0x21FE)).thenReturn (0x34)
        when (cpu.getMemory (0x21FF)).thenReturn (0x56)
        val result = subject.mods (cpu)

        it ("does so appropriately") {
          assert (result === "IP: $1000 -> $123456; SP: $21FC -> $21FF")
        }
      }
    }

    describe ("a Pop") {
      val subject = Pop (0x42)

      describe ("directed to show mods") {
        when (cpu.sp).thenReturn (0x21FF)
        when (cpu.getMemory (0x42)).thenReturn (0x12)
        when (cpu.getMemory (0x2200)).thenReturn (0x24)
        val result = subject.mods (cpu)

        it ("does so appropriately") {
          assert (result === "($42): $12 -> $24; SP: $21FF -> $2200")
        }
      }
    }

    describe ("a SetSp") {
      val subject = SetSp (0x4224)

      describe ("when executed") {
        subject.execute (cpu)

        it ("performs appropriately") {
          verify (cpu).sp_= (0x4224)
        }
      }

      describe ("directed to show mods") {
        when (cpu.sp).thenReturn (0x21FF)
        val result = subject.mods (cpu)

        it ("does so appropriately") {
          assert (result === "SP: $21FF -> $4224")
        }
      }
    }

    describe ("a SetMemory") {
      val subject = SetMemory (0x1234, 0x42)

      describe ("when executed") {
        subject.execute (cpu)

        it ("performs appropriately") {
          verify (cpu).setMemory (0x1234, 0x42)
        }
      }

      describe ("directed to show mods") {
        when (cpu.getMemory (0x1234)).thenReturn (0x24)
        val result = subject.mods (cpu)

        it ("does so appropriately") {
          assert (result === "($1234): $24 -> $42")
        }
      }
    }

    describe ("a SetFlags") {

      describe ("when executed") {
        when (cpu.getMemory (SREG)).thenReturn (0xF0) // 1111 0000 - original
        val subject = SetFlags (0x66,                 // 0110 0110 - mask
                                0x0F)                 // 0000 1111 - new value
        subject.execute (cpu)

        it ("performs appropriately") {
          verify (cpu).setMemory (SREG, 0x96)         // 1001 0110 - result
        }
      }

      describe ("directed to show mods") {
        val subject = SetFlags (0xFF, 0xAA)
        when (cpu.getMemory (RegisterNames.SREG)).thenReturn (0x55)
        val result = subject.mods (cpu)

        it ("does so appropriately") {
          assert (result === "SREG: iThSvNzC -> ItHsVnZc")
        }
      }
    }
  }
}
