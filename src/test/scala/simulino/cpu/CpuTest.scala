package simulino.cpu

import org.scalatest.path
import org.mockito.Mockito._
import org.mockito.Matchers._
import simulino.engine.{Event, Engine}
import simulino.memory.{UnsignedByte, Memory}

/**
 * Created by dnwiebe on 5/11/15.
 */
class CpuTest extends path.FunSpec {
  describe ("A Cpu") {
    class TestCpu (val engine: Engine) extends Cpu {
      val programMemory = new Memory (2000)
      val config = null
      val instruction = mock (classOf[Instruction[TestCpu]])
      when (instruction.execute (any (classOf[TestCpu]))).thenReturn (Nil)
      val instructionSet = new InstructionSet[TestCpu] () {
        override def apply (data: Array[UnsignedByte]) = Some (instruction)
      }
    }
    val engine = mock (classOf[Engine])
    val subject = new TestCpu (engine)

    it ("starts with 0 in the IP") {
      assert (subject.ip === 0)
    }

    describe ("given an Instruction") {
      val oneConsequence = mock (classOf[Event])
      val anotherConsequence = mock (classOf[Event])
      val instruction = mock (classOf[Instruction[TestCpu]])
      when (instruction.latency).thenReturn (5)
      when (instruction.execute (subject)).thenReturn (List (oneConsequence, anotherConsequence))
      subject.receive (instruction)

      it ("executes the instruction with the Cpu and schedules the consequences") {
        verify (engine).schedule (oneConsequence, 5)
        verify (engine).schedule (anotherConsequence, 5)
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
