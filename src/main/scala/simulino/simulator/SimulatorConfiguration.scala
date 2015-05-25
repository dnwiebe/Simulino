package simulino.simulator

import java.io.InputStream
import simulino.cpu.Cpu
import simulino.utils.Utils._
import com.fasterxml.jackson.databind.{JsonNode, ObjectMapper}

/**
 * Created by dnwiebe on 5/10/15.
 */

case object MemoryConfiguration {
  def apply (node: JsonNode): MemoryConfiguration = {
    val programSize = node.get ("program").asInt
    val dynamicSize = node.get ("dynamic").asInt
    val persistentSize = node.get ("persistent").asInt
    new MemoryConfiguration (programSize, dynamicSize, persistentSize)
  }
}

case class MemoryConfiguration (
  programSize: Int,
  dynamicSize: Int,
  persistentSize: Int
)

case object CpuConfiguration {
  def apply (node: JsonNode): CpuConfiguration = {
    val clockSpeed = node.get ("clockSpeed").asInt
    val className = node.get ("class").asText
    val cls = Class.forName (className).asInstanceOf[Class[Cpu]]
    val classSpecific = node.get ("classSpecific")
    new CpuConfiguration (clockSpeed, cls, classSpecific)
  }

  private def debugHere () {}
}

case class CpuConfiguration (
  clockSpeed: Int,
  cls: Class[_ <: Cpu],
  classSpecific: JsonNode
)

case object SimulatorConfiguration {
  def apply (istr: InputStream): SimulatorConfiguration = {
    val mapper = new ObjectMapper ()
    val root = mapper.readTree (istr)
    apply (root)
  }

  def apply (node: JsonNode): SimulatorConfiguration = {
    SimulatorConfiguration (
      memory = MemoryConfiguration (node.get ("memory")),
      cpu = CpuConfiguration (node.get ("cpu"))
    )
  }
}

case class SimulatorConfiguration (
  memory: MemoryConfiguration = null,
  cpu: CpuConfiguration = null
)
