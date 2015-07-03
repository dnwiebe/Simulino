package simulino.simulator

import java.io.{ByteArrayInputStream, StringReader}

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.databind.node.ObjectNode
import org.scalatest.path
import org.mockito.Mockito._
import org.mockito.Matchers._
import simulino.cpu.arch.avr.AvrCpu
import simulino.cpu.{Instruction, InstructionSet, Cpu}
import simulino.engine.{ScheduledEvent, Subscriber, Engine}
import simulino.memory.{Memory, Span, UnsignedByte}
import simulino.simulator.peripheral.PinSampler
import simulino.utils.TestUtils._

import scala.collection.mutable.ListBuffer

/**
 * Created by dnwiebe on 5/10/15.
 */
class TestCpu (val engine: Engine, val programMemory: Memory, val config: CpuConfiguration) extends Cpu {
  val instructionSet = mock (classOf[InstructionSet[TestCpu]])
  val instruction = mock (classOf[Instruction[TestCpu]])
  when (instruction.execute (any (classOf[TestCpu]))).thenReturn (Nil)
  when (instructionSet.apply (any (classOf[Array[UnsignedByte]]))).thenReturn (Some (instruction))
}

class ExecutionLogTest extends path.FunSpec {
  describe ("An ExecutionLog with big numbers") {
    val subject = ExecutionLog (16000000000L, 262144, "INST, Ruc, TI(0)N", "modified stuff")

    describe ("when directed to render itself as a string") {
      val result = subject.toString

      it ("does so properly"){
        assert (result === "16000000000:  40000 INST, Ruc, TI(0)N    ; modified stuff")
      }
    }
  }

  describe ("An ExecutionLog with little numbers") {
    val subject = ExecutionLog (0L, 0, "XY", "nothing")

    describe ("when directed to render itself as a string") {
      val result = subject.toString

      it ("does so properly"){
        assert (result === "          0:      0 XY                   ; nothing")
      }
    }
  }
}

class SimulatorTest extends path.FunSpec {

  val istr = new ByteArrayInputStream (
    """
      |{
      |  "memory": {
      |    "internal": 512,
      |    "sram": 32
      |  },
      |  "portMap": {
      |    "ports": [],
      |    "portHandlerClasses": []
      |  },
      |  "interruptVectors": []
      |}
    """.stripMargin.getBytes)
  val mapper = new ObjectMapper ()
  val cpuNode = mapper.readTree (istr)
  val configuration = new SimulatorConfiguration (
    memory = new MemoryConfiguration (1024),
    chipPinFor = Map ("Board Pin" -> "Chip Pin"),
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

      describe ("when directed to provide a PinSampler") {
        val cpu = mock (classOf[Cpu])
        val expected = new PinSampler (1000)
        when (cpu.pinSampler ("Chip Pin")).thenReturn (expected)
        subject.cpu = cpu
        val result = subject.pinSampler ("Board Pin")

        it ("passes the buck to its cpu") {
          assert (result === expected)
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

      describe ("when given an execution logger") {
        val recording = ListBuffer[ExecutionLog] ()
        val logger = {log: ExecutionLog => recording.append (log)}
        subject.setExecutionLogger (logger)

        describe ("and the CPU's logger is called a couple of times") {
          subject.cpu.logInstruction.get (ExecutionLog (1, 2, "3", "4"))
          subject.cpu.logInstruction.get (ExecutionLog (5, 6, "7", "8"))

          it ("the provided logger is called") {
            assert (recording.toList === List (
              ExecutionLog (1, 2, "3", "4"),
              ExecutionLog (5, 6, "7", "8")
            ))
          }
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
    val memConfig = new MemoryConfiguration (10)
    val cpuConfig = new CpuConfiguration (1000, classOf[AvrCpu], cpuNode)
    val config = new SimulatorConfiguration (memConfig, cpuConfig)
    val subject = new Simulator (config)

    describe ("with a real four-byte program in memory") {
      val rdr = new StringReader (":04000000010CFECF22\n:00000001FF")
      subject.loadHex (rdr)

      describe ("and ones in the first two registers") {
        subject.cpu.asInstanceOf[AvrCpu].setMemory (0, 1)
        subject.cpu.asInstanceOf[AvrCpu].setMemory (1, 1)

        describe ("when run for four iterations through the loop") {
          subject.runForTicks (12)

          it ("register 0 has been incremented to 5") {
            assert (subject.cpu.asInstanceOf[AvrCpu].getMemory (0) === UnsignedByte (5))
          }
        }
      }
    }
  }
}
