package simulino.cpu

import org.mockito.{ArgumentCaptor, Matchers}
import org.scalatest.path
import org.mockito.Mockito._
import simulino.cpu.arch.avr.ATmega.DEC
import simulino.engine.Engine
import simulino.memory.{UnsignedByte, Memory}

/**
 * Created by dnwiebe on 6/7/15.
 */
class CpuChangeTest extends path.FunSpec {
  describe ("Given a mock cpu") {
    val cpu = mock (classOf[Cpu])

    describe ("an IncrementIp") {
      val subject = IncrementIp (2)

      describe ("executed") {
        when (cpu.ip).thenReturn (0x1000)
        subject.execute (cpu)

        it ("performs appropriately") {
          verify (cpu).ip_= (0x1002)
        }
      }

      describe ("asked for mods") {
        when (cpu.ip).thenReturn (0x1000)
        val result = subject.mods (cpu)

        it ("responds appropriately") {
          assert (result === "IP: $1000 -> $1002")
        }
      }
    }

    describe ("a SetIp") {
      val subject = SetIp (0x180)

      describe ("executed") {
        subject.execute (cpu)

        it ("performs appropriately") {
          verify (cpu).ip_= (0x180)
        }
      }

      describe ("asked for mods") {
        when (cpu.ip).thenReturn (0x1000)
        val result = subject.mods (cpu)

        it ("responds appropriately") {
          assert (result === "IP: $1000 -> $180")
        }
      }
    }

    describe ("a ScheduleNextInstruction") {
      val subject = ScheduleNextInstruction ()

      describe ("executed") {
        val engine = mock (classOf[Engine])
        when (engine.currentTick).thenReturn (47L)
        when (cpu.engine).thenReturn (engine)
        when (cpu.ip).thenReturn (0x1000)
        val instruction = mock (classOf [Instruction[Cpu]])
        doReturn (instruction).when (cpu).instructionAt (0x1000)
        subject.execute (cpu)

        it ("performs appropriately") {
          val captor = ArgumentCaptor.forClass (classOf[DEC])
          verify (engine, times (1)).schedule (captor.capture (), Matchers.eq (47L))
          assert (captor.getValue === instruction)
        }
      }

      describe ("asked for mods") {
        val result = subject.mods (null)

        it ("returns blank") {
          assert (result === "")
        }
      }
    }
  }
}
