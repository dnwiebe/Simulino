package simulino.cpu

import org.scalatest.path
import org.mockito.Mockito._
import simulino.engine.{Event, Engine}

/**
 * Created by dnwiebe on 5/11/15.
 */
class CpuTest extends path.FunSpec {
  describe ("A Cpu") {
    class TestCpu (val engine: Engine) extends Cpu {
      val config = null
    }
    val engine = mock (classOf[Engine])
    val subject = new TestCpu (engine)

    it ("starts with 0 in the IP") {
      assert (subject.ip === 0)
    }

    it ("starts with 0 in the SP") {
      assert (subject.sp === 0)
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

    describe ("directed to set SP") {
      subject.receive (SetSp (1000))

      it ("does so") {
        assert (subject.sp === 1000)
      }

      describe ("and then increment it by -2") {
        subject.receive (IncrementSp (-2))

        it ("does so") {
          assert (subject.sp === 998)
        }
      }
    }
  }
}
