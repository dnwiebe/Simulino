package simulino.simulator

import java.io.InputStream
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
    new CpuConfiguration (clockSpeed)
  }
}

case class CpuConfiguration (
  clockSpeed: Int
)

case object SimulatorConfiguration {
  def apply (istr: InputStream): SimulatorConfiguration = {
    def mapper = new ObjectMapper ()
    def root = mapper.readTree (istr)
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
  memory: MemoryConfiguration,
  cpu: CpuConfiguration
)
