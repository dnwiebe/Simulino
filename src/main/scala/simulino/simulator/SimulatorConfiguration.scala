package simulino.simulator

import java.io.InputStream
import com.fasterxml.jackson.databind.node.{ObjectNode, ArrayNode}
import simulino.cpu.Cpu
import simulino.utils.Utils._
import com.fasterxml.jackson.databind.{JsonNode, ObjectMapper}
import scala.collection.JavaConverters._

/**
 * Created by dnwiebe on 5/10/15.
 */

class JsonConfigurationException (msg: String) extends RuntimeException (msg)

case object MemoryConfiguration {
  def apply (node: JsonNode): MemoryConfiguration = {
    val programSize = hexOrDec (node.get ("program"))
    new MemoryConfiguration (programSize)
  }
}

case class MemoryConfiguration (
  programSize: Int
)

case object CpuConfiguration {
  def apply (node: JsonNode): CpuConfiguration = {
    val clockSpeed = node.get ("clockSpeed").asInt
    val vcc = node.get ("vcc").asDouble
    val className = node.get ("class").asText
    val cls = Class.forName (className).asInstanceOf[Class[Cpu]]
    val classSpecific = node.get ("classSpecific")
    new CpuConfiguration (clockSpeed, vcc, cls, classSpecific)
  }

  private def debugHere () {}
}

case class CpuConfiguration (
  clockSpeed: Int,
  vcc: Double,
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
    val (boardPinFor, chipPinFor) = makePinMaps (node.get ("pinMap").asInstanceOf[ArrayNode])
    SimulatorConfiguration (
      memory = MemoryConfiguration (node.get ("memory")),
      boardPinFor = boardPinFor,
      chipPinFor = chipPinFor,
      cpu = CpuConfiguration (node.get ("cpu"))
    )
  }

  private def makePinMaps (node: ArrayNode): (Map[String, String], Map[String, String]) = {
    if (node == null) {(Map(), Map())}
    else {
      val pairsPair = node.elements ().asScala.foldLeft ((List[(String, String)] (), List[(String, String)] ())) {(soFar, subnode) =>
        val (boardPinFor, chipPinFor) = soFar
        val pairNode = subnode.asInstanceOf[ObjectNode]
        val boardPin = pairNode.get ("boardPin").asText
        val chipPin = pairNode.get ("chipPin").asText
        ((chipPin, boardPin) :: boardPinFor, (boardPin, chipPin) :: chipPinFor)
      }
      (pairsPair._1.toMap, pairsPair._2.toMap)
    }
  }
}

case class SimulatorConfiguration (
  memory: MemoryConfiguration = null,
  cpu: CpuConfiguration = null,
  boardPinFor: Map[String, String] = null,
  chipPinFor: Map[String, String] = null
)
