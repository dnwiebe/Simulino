package simulino.simulator

import java.io.StringReader

import com.fasterxml.jackson.databind.node.ObjectNode
import org.scalatest.path
import org.mockito.Mockito._
import org.mockito.Matchers._
import simulino.cpu.{Instruction, InstructionSet, Cpu}
import simulino.cpu.arch.AvrCpu
import simulino.engine.{ScheduledEvent, Subscriber, Engine}
import simulino.memory.{Memory, Span, UnsignedByte}
import simulino.utils.TestUtils._

/**
 * Created by dnwiebe on 5/10/15.
 */
class TestCpu (val engine: Engine, val programMemory: Memory, val config: CpuConfiguration) extends Cpu {
  val instructionSet = mock (classOf[InstructionSet[TestCpu]])
  val instruction = mock (classOf[Instruction[TestCpu]])
  when (instruction.execute (any (classOf[TestCpu]))).thenReturn (Nil)
  when (instructionSet.apply (any (classOf[Array[UnsignedByte]]))).thenReturn (Some (instruction))
}

class SimulatorTest extends path.FunSpec {

  val cpuNode = new ObjectNode (null)
  val configuration = new SimulatorConfiguration (
    memory = new MemoryConfiguration (1024, 256, 16),
    cpu = new CpuConfiguration (clockSpeed = 1000, cls = classOf[TestCpu], classSpecific = cpuNode)
  )

  describe ("A Simulator") {
    val subject = new Simulator (configuration)

    describe ("compared with a different simulator of the same configuration") {
      val another = new Simulator (configuration)

      it ("has a different ID") {
        assert (subject.id != another.id)
      }
    }

    describe ("when it comes to memory dumps") {
      describe ("asked for a dump of its program memory") {
        val result = subject.dumpProgramMemory (0, 256)

        it ("responds with zeros") {
          assert (result === (0 until 256).map { i => UnsignedByte (0) }.toArray)
        }
      }

      describe ("asked for a dump of its dynamic memory") {
        val result = subject.dumpDynamicMemory (0, 1024)

        it ("responds with zeros") {
          assert (result === (0 until 1024).map { i => UnsignedByte (0) }.toArray)
        }
      }

      describe ("asked for a dump of its persistent memory") {
        val result = subject.dumpPersistentMemory (0, 16)

        it ("responds with zeros") {
          assert (result === (0 until 16).map { i => UnsignedByte (0) }.toArray)
        }
      }
    }

    describe ("when it comes to CPUs") {
      describe ("has a CPU with the proper") {
        it ("class") {
          assert (subject.cpu.getClass === classOf[TestCpu])
        }

        it ("clock speed") {
          assert (subject.cpu.config.clockSpeed === 1000)
        }

        it ("class-specific config") {
          assert (subject.cpu.config.classSpecific === cpuNode)
        }
      }
    }

    describe ("given a .hex file with a couple of buffers") {
      val hex = new StringReader (
        ":0400000012345678E8\n" +
        ":0402000087654321AA\n" +
        ":00000001FF\n"
      )
      subject.loadHex (hex)

      it ("loads the first buffer properly") {
        assert (subject.dumpProgramMemory (0, 5) === unsignedBytes (0x12, 0x34, 0x56, 0x78, 0x00))
      }

      it ("loads the second buffer properly") {
        assert (subject.dumpProgramMemory (0x1FF, 6) === unsignedBytes (0x00, 0x87, 0x65, 0x43, 0x21, 0x00))
      }
    }

    describe ("outfitted with a mock Engine") {
      val engine = mock (classOf[Engine])
      subject.engine = engine

      describe ("and given a Subscriber") {
        val subscriber = mock (classOf[Subscriber])
        subject.addSubscriber (subscriber)

        it ("passes the subscriber on to the Engine") {
          verify (engine).addSubscriber (subscriber)
        }

        describe ("and directed to schedule an event") {
          val event = mock (classOf[ScheduledEvent])
          subject.schedule (event)

          it ("passes the schedule on to the Engine") {
            verify (engine).schedule (event)
          }
        }
      }

      describe ("and directed to run for some time") {
        subject.runForSeconds (0.010)

        it ("ticks the Engine over") {
          verify (engine, times (10)).tick ()
        }
      }
    }
  }

  describe ("A Simulator with a real AvrCpu") {
    val memConfig = new MemoryConfiguration (10, 0, 0)
    val cpuConfig = new CpuConfiguration (1000, classOf[AvrCpu], null)
    val config = new SimulatorConfiguration (memConfig, cpuConfig)
    val subject = new Simulator (config)

    describe ("with a real four-byte program in memory") {
      val rdr = new StringReader (":04000000010CFECF22\n:00000001FF")
      subject.loadHex (rdr)

      describe ("and ones in the first two registers") {
        subject.cpu.asInstanceOf[AvrCpu].setRegister (0, 1)
        subject.cpu.asInstanceOf[AvrCpu].setRegister (1, 1)

        describe ("when run for four iterations through the loop") {
          subject.runForTicks (12)

          it ("register 0 has been incremented to 4") {
            assert (subject.cpu.asInstanceOf[AvrCpu].register (0) === UnsignedByte (4))
          }
        }
      }
    }
  }
}
