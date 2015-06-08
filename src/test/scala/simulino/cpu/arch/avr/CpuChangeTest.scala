package simulino.cpu.arch.avr

import org.scalatest.path
import org.mockito.Mockito._

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
        when (cpu.register (0x21FF)).thenReturn (0x65)
        when (cpu.register (0x21FE)).thenReturn (0x43)
        when (cpu.register (0x21FD)).thenReturn (0x21)
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
        when (cpu.register (0x21FF)).thenReturn (0x12)
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
        when (cpu.register (0x21FD)).thenReturn (0x12)
        when (cpu.register (0x21FE)).thenReturn (0x34)
        when (cpu.register (0x21FF)).thenReturn (0x56)
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
        when (cpu.register (0x42)).thenReturn (0x12)
        when (cpu.register (0x2200)).thenReturn (0x24)
        val result = subject.mods (cpu)

        it ("does so appropriately") {
          assert (result === "($42): $12 -> $24; SP: $21FF -> $2200")
        }
      }
    }

    describe ("a SetSp") {
      val subject = SetSp (0x4224)

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

      describe ("directed to show mods") {
        when (cpu.register (0x1234)).thenReturn (0x24)
        val result = subject.mods (cpu)

        it ("does so appropriately") {
          assert (result === "($1234): $24 -> $42")
        }
      }
    }

    describe ("a SetFlags") {
      val subject = SetFlags (0xFF, 0xAA)

      describe ("directed to show mods") {
        when (cpu.register (RegisterNames.SREG)).thenReturn (0x55)
        val result = subject.mods (cpu)

        it ("does so appropriately") {
          assert (result === "SREG: iThSvNzC -> ItHsVnZc")
        }
      }
    }
  }
}
