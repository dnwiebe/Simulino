package simulino.simulator

import java.io.{ByteArrayInputStream, StringReader}

import org.scalatest.path

/**
 * Created by dnwiebe on 5/10/15.
 */
class SimulatorConfigurationTest extends path.FunSpec {
  describe ("A SimulatorConfiguration produced from JSON") {
    val json =
    """{
      | "memory": {
      |   "program": 1234,
      |   "dynamic": 2345,
      |   "persistent": 3456
      | },
      | "cpu": {
      |   "clockSpeed": 16000000
      | }
      |}
    """.stripMargin
    val subject = SimulatorConfiguration (new ByteArrayInputStream (json.getBytes))

    it ("has the expected memory sizes") {
      assert (subject.memory.programSize === 1234)
      assert (subject.memory.dynamicSize === 2345)
      assert (subject.memory.persistentSize === 3456)
    }

    it ("has the expected CPU attributes") {
      assert (subject.cpu.clockSpeed === 16000000)
    }
  }
}
