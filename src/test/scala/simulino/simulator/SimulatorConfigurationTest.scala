package simulino.simulator

import java.io.{ByteArrayInputStream, StringReader}

import org.scalatest.path
import simulino.cpu.{InstructionSet, Cpu}
import simulino.engine.Engine
import simulino.memory.Memory

/**
 * Created by dnwiebe on 5/10/15.
 */
class SimulatorConfigurationTest extends path.FunSpec {

  class TestCpu (val clockSpeed: Int, val config: CpuConfiguration, val engine: Engine) extends Cpu {
    override val programMemory: Memory = null
    override val instructionSet: InstructionSet = null
  }

  describe ("A SimulatorConfiguration produced from JSON") {
    val json =
    s"""{
      | "memory": {
      |   "program": 1234
      | },
      | "pinMap": [
      |   {"boardPin": "Murray", "chipPin": "Rothbard"},
      |   {"boardPin": "Frank", "chipPin": "Chodorov"}
      | ],
      | "cpu": {
      |   "clockSpeed": 16000000,
      |   "vcc": 7.5,
      |   "class": "${classOf[TestCpu].getName}",
      |   "classSpecific": {
      |     "cat": "Mr. Bigglesworth"
      |   }
      | }
      |}
    """.stripMargin
    val subject = SimulatorConfiguration (new ByteArrayInputStream (json.getBytes))

    it ("has the expected memory size") {
      assert (subject.memory.programSize === 1234)
    }

    it ("has the expected pin map") {
      assert (subject.boardPinFor ("Rothbard") === "Murray")
      assert (subject.boardPinFor ("Chodorov") === "Frank")
      assert (subject.chipPinFor ("Murray") === "Rothbard")
      assert (subject.chipPinFor ("Frank") === "Chodorov")
    }

    it ("has the expected CPU attributes") {
      assert (subject.cpu.clockSpeed === 16000000)
      assert (subject.cpu.vcc === 7.5)
      assert (subject.cpu.cls === classOf[TestCpu])
      assert (subject.cpu.classSpecific.get ("cat").asText () === "Mr. Bigglesworth")
    }
  }
}
