package simulino.cpu

import org.scalatest.path
import org.mockito.Mockito._
import org.mockito.Matchers._
import simulino.engine.{Event, Engine}
import simulino.memory.{UnsignedByte, Memory}
import simulino.simulator.CpuConfiguration

/**
 * Created by dnwiebe on 5/11/15.
 */
class CpuTest extends path.FunSpec {
  describe ("A Cpu") {
    class TestCpu (val engine: Engine) extends Cpu {
      val programMemory = new Memory (2000)
      val config = new CpuConfiguration(16000000, 6.3,classOf[TestCpu], null)
      val instruction = mock (classOf[Instruction[TestCpu]])
      when (instruction.execute (any (classOf[TestCpu]))).thenReturn (Nil)
      val instructionSet = new InstructionSet () {
        override def apply (data: Array[UnsignedByte]) = Some (instruction)
      }
    }
    val engine = mock (classOf[Engine])
    val subject = new TestCpu (engine)

    it ("starts with 0 in the IP") {
      assert (subject.ip === 0)
    }

    describe ("directed to produce a couple of pin samplers") {
      val oneSampler = subject.pinSampler ("one")
      val anotherSampler = subject.pinSampler ("another")
      val anotherAnotherSampler = subject.pinSampler ("another")
      when (engine.currentTick).thenReturn (1000)

      it ("they have the proper clock speed") {
        assert (oneSampler.clockSpeed === 16000000)
        assert (anotherSampler.clockSpeed === 16000000)
        assert (anotherAnotherSampler.clockSpeed === 16000000)
      }

      describe ("and stimulated on one pin") {
        subject.showVoltageAtPin ("one", Some (2.0))

        it ("shows the voltage change on that pin") {
          assert (oneSampler.sampleAtTick (999) === None)
          assert (oneSampler.sampleAtTick (1000) === Some (2.0))
        }

        it ("does not show the voltage change on the other") {
          assert (anotherSampler.sampleAtTick (1000) === None)
          assert (anotherAnotherSampler.sampleAtTick (1000) === None)
        }
      }

      describe ("and stimulated on another pin") {
        subject.showVoltageAtPin ("another", Some (2.0))

        it ("shows the voltage change on the other pin") {
          assert (anotherSampler.sampleAtTick (999) === None)
          assert (anotherSampler.sampleAtTick (1000) === Some (2.0))
          assert (anotherAnotherSampler.sampleAtTick (999) === None)
          assert (anotherAnotherSampler.sampleAtTick (1000) === Some (2.0))
        }

        it ("does not show the voltage change on the one pin") {
          assert (oneSampler.sampleAtTick (1000) === None)
        }
      }
    }

    describe ("given an Instruction") {
      val oneConsequence = mock (classOf[Event])
      val anotherConsequence = mock (classOf[Event])
      val instruction = mock (classOf[Instruction[TestCpu]])
      when (instruction.length).thenReturn (3)
      when (instruction.latency).thenReturn (5)
      when (instruction.execute (subject)).thenReturn (List (oneConsequence, anotherConsequence))
      subject.receive (instruction)

      it ("executes the instruction with the Cpu, schedules the consequences, and prepares the next instruction") {
        val order = inOrder (engine)
        order.verify (engine).schedule (IncrementIp (3), 5)
        order.verify (engine).schedule (oneConsequence, 5)
        order.verify (engine).schedule (anotherConsequence, 5)
        order.verify (engine).schedule (ScheduleNextInstruction (), 5)
      }
    }

    describe ("directed to schedule an instruction") {
      when (engine.currentTick).thenReturn (1000)
      when (engine.nextTick).thenReturn (1001)
      subject.receive (ScheduleNextInstruction ())

      it ("does so") {
        verify (engine).schedule (subject.instruction, 1000)
      }
    }

    describe ("directed to set IP") {
      subject.receive (SetIp (1000))

      it ("does so") {
        assert (subject.ip === 1000)
      }

      describe ("and then increment it by -2") {
        subject.receive (IncrementIp (-2))

        it ("does so") {
          assert (subject.ip === 998)
        }
      }
    }
  }
}
