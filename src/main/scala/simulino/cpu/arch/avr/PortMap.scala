package simulino.cpu.arch.avr

import com.fasterxml.jackson.databind.JsonNode
import com.fasterxml.jackson.databind.node.ArrayNode
import simulino.cpu.arch.avr.ATmega.PortType
import simulino.engine.TickSink
import simulino.memory.{UnsignedByte, Memory}
import simulino.utils.Utils._
import scala.collection.JavaConverters._

/**
 * Created by dnwiebe on 5/23/15.
 */

trait Subscriber {
  def outgoingChange (name: String, value: Int)
}

case class Port (name: String, address: Int, lowBit: Int, bitLength: Int, direction: PortType) {
  private val mask = ((1 << bitLength) - 1) << lowBit

  def affectedByChange (oldValue: UnsignedByte, newValue: UnsignedByte): Boolean = {
    (oldValue.value & mask) != (newValue.value & mask)
  }

  def read (byte: UnsignedByte): Int = {
    (byte.value & mask) >> lowBit
  }

  def write (previous: UnsignedByte, value: Int): UnsignedByte = {
    val shiftedNewValue = (value << lowBit) & mask
    val clearedPreviousValue = previous.value & ~mask
    UnsignedByte (clearedPreviousValue | shiftedNewValue)
  }
}

trait PortHandler {
  private var initialized = false
  private var cpu: AvrCpu = null
  val name: String
  val portNames: Seq[String]

  def initialize (cpu: AvrCpu): Unit = {
    if (initialized) {throw new IllegalStateException (s"${getClass.getName} may not be reinitialized")}
    this.cpu = cpu
    initialized = true
  }

  def acceptChange (portName: String, oldValue: Int, newValue: Int): Unit = {}

  protected def writeToPort (name: String, value: Int): Unit = {
    if (!portNames.contains (name)) {
      throw new IllegalArgumentException (s"${getClass.getName} attempted write to unrequested port ${name}")
    }
    cpu.portMap.writeToPort (name, value)
  }

  protected def readFromPort (name: String): Int = {
    if (!portNames.contains (name)) {
      throw new IllegalArgumentException (s"${getClass.getName} attempted read from unrequested port ${name}")
    }
    cpu.portMap.readFromPort (name)
  }

  protected def raiseInterrupt (name: String): Unit = {
    cpu.raiseInterrupt (name)
  }
}

case object PortConfiguration {
  def apply (node: JsonNode): PortConfiguration = {
    val ports = node.get ("ports").asInstanceOf[ArrayNode].elements.asScala.map {item =>
      (
        item.get ("name").asText (),
        Port (
          item.get ("name").asText (),
          hexOrDec (item.get ("address")),
          hexOrDec (item.get ("lowBit")),
          hexOrDec (item.get ("bitLength")),
          PortType.fromChar (item.get ("direction").asText ().charAt (0))
        )
      )
    }.toList
    val portHandlerClasses = node.get ("portHandlerClasses").asInstanceOf[ArrayNode].elements.asScala.map {item =>
      Class.forName (item.asText ()).asInstanceOf[Class[PortHandler]]
    }.toList
    new PortConfiguration (
      ports,
      portHandlerClasses
    )
  }
}

case class PortConfiguration (
  ports: Seq[(String, Port)],
  portHandlerClasses: Seq[Class[PortHandler]]
)

object PortMap {

  def validatePortHandler (handler: PortHandler, ports: Map[String, Port]): Unit = {
    val missingPorts = handler.portNames.foldLeft (List[Option[String]] ()) {(soFar, portName) =>
      ports.contains (portName) match {
        case true => None :: soFar
        case false => Some (portName) :: soFar
      }
    }.flatten.reverse
    val msgFragment = missingPorts match {
      case Nil => null
      case x :: Nil => s"port ${x}"
      case xs => s"ports ${xs.mkString (", ")}"
    }
    if (msgFragment != null)
      throw new IllegalArgumentException (s"PortHandler ${handler.getClass.getName} requires nonexistent ${msgFragment}")
  }
}

class PortMap (cpu: AvrCpu, configs: List[PortConfiguration]) {
  import PortMap._

  private val portsByName: Map[String, Port] = extractPortsByName (configs)
  private val portsByAddress: Map[Int, Seq[Port]] = groupPortsByAddress (portsByName.map {(p) => p._2}.toList)
  private val handlersByName: Map[String, PortHandler] = extractHandlersByName (configs)
  private val handlersByPortName: Map[String, Seq[PortHandler]] = groupHandlersByPortName (handlersByName.map {p => p._2}.toList)

  def handler (name: String): Option[PortHandler] = handlersByName.get (name)

  def writeToPort (name: String, value: Int): Unit = {
    val port = portsByName (name)
    val oldValue = cpu.dataMemory.getData (port.address, 1)(0)
    val newValue = port.write (oldValue, value)
    cpu.dataMemory.update (port.address, newValue)
  }

  def readFromPort (name: String): Int = {
    val port = portsByName (name)
    val byte = cpu.dataMemory.getData (port.address, 1)(0)
    port.read (byte)
  }

  def memoryChange (address: Int, oldValue: UnsignedByte, newValue: UnsignedByte): Unit = {
    val portsForAddress = portsByAddress.getOrElse (address, Nil)
    val portsAffected = portsForAddress.filter {_.affectedByChange (oldValue, newValue)}
    portsAffected.foreach {port =>
      val handlersAffected = handlersByPortName.getOrElse (port.name, Nil)
      handlersAffected.foreach {_.acceptChange (port.name, oldValue, newValue)}
    }
  }

  private def extractPortsByName (configs: Seq[PortConfiguration]): Map[String, Port] = {
    configs.foldLeft (List[(String, Port)] ()) {(soFar, config) => soFar ++ config.ports}.toMap
  }

  private def groupPortsByAddress (ports: Seq[Port]): Map[Int, Seq[Port]] = {
    ports.foldLeft (Map[Int, List[Port]] ()) {(soFar, port) =>
      soFar.get (port.address) match {
        case Some (seq) => soFar + (port.address -> (port :: seq))
        case None => soFar + (port.address -> (port :: Nil))
      }
    }
  }

  private def extractHandlersByName (configs: Seq[PortConfiguration]): Map[String, PortHandler] = {
    val classes = configs.foldLeft (List[Class[PortHandler]] ()) {(soFar, config) =>
      soFar ++ config.portHandlerClasses
    }
    classes.map {cls =>
      val handler = cls.newInstance ()
      handler.initialize (cpu)
      validatePortHandler (handler, portsByName)
      if (classOf[TickSink].isAssignableFrom (handler.getClass)) {
        cpu.engine.addTickSink (handler.asInstanceOf[TickSink])
      }
      (handler.name, handler)
    }.toMap
  }

  private def groupHandlersByPortName (handlers: Seq[PortHandler]): Map[String, Seq[PortHandler]] = {
    val portNameHandlerPairs = handlers.foldLeft (Seq[(String, PortHandler)] ()) {(soFar, handler) =>
      soFar ++ handler.portNames.map {(_, handler)}
    }
    portNameHandlerPairs.foldLeft (Map[String, List[PortHandler]] ()) {(soFar, pair) =>
      val (portName, handler) = pair
      soFar.get (portName) match {
        case Some (handlers) => soFar + (portName -> (handler :: handlers))
        case None => soFar + (portName -> (handler :: Nil))
      }
    }
  }
}
