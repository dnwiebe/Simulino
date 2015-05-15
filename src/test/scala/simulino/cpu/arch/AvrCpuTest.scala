package simulino.cpu.arch

import org.scalatest.path
import org.mockito.Mockito._
import simulino.engine.Engine
import simulino.memory.{UnsignedByte, Span, Memory}
import simulino.simulator.CpuConfiguration
import simulino.utils.TestUtils._

/**
 * Created by dnwiebe on 5/14/15.
 */
class AvrCpuTest extends path.FunSpec {
  describe ("An AvrCpu") {
    val memory = new Memory (16)
    memory.addSpan (Span (0, unsignedBytes (0x0C, 0x01, 0xCF, 0xFC)))
    val engine = mock (classOf[Engine])
    val config = mock (classOf[CpuConfiguration])

    val subject = new AvrCpu (engine, config)

    describe ("initially has zeros in all registers") {
      (0 until 32).foreach {i => assert (subject.register (i) === UnsignedByte (0), s"Register ${i}")}
    }
  }
}
