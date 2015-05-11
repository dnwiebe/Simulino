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
      val config = null; val programMemory = null; val dynamicMemory = null; val pinout = null
    }
    val engine = mock (classOf[Engine])
    val subject = new TestCpu (engine)

    describe ("given an Instruction") {
      val oneConsequence = mock (classOf[Event])
      val anotherConsequence = mock (classOf[Event])
      val instruction = mock (classOf[Instruction])
      when (instruction.execute (subject)).thenReturn (List (oneConsequence, anotherConsequence))
      subject.receive (instruction)

      it ("executes the instruction with the Cpu and schedules the consequences") {
        verify (engine).schedule (oneConsequence)
        verify (engine).schedule (anotherConsequence)
      }
    }

    describe ("given a CpuChange") {
      val oneConsequence = mock (classOf[Event])
      val anotherConsequence = mock (classOf[Event])
      val change = mock (classOf[CpuChange])
      when (change.execute (subject)).thenReturn (List (oneConsequence, anotherConsequence))
      subject.receive (change)

      it ("executes the change with the Cpu and schedules the consequences") {
        verify (engine).schedule (oneConsequence)
        verify (engine).schedule (anotherConsequence)
      }
    }
  }
}
