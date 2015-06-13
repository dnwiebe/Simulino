package simulino.cpu.arch.avr

import java.lang.reflect.{Parameter, Constructor}

import com.fasterxml.jackson.databind.JsonNode
import com.fasterxml.jackson.databind.node.{ObjectNode, ArrayNode}
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
  val portNames: List[String]

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

  protected def showVoltageAtPin (chipPin: String, voltage: Option[Double]) {
    cpu.showVoltageAtPin (chipPin, voltage)
  }

  protected def raiseInterrupt (name: String): Unit = {
    cpu.raiseInterrupt (name)
  }
}

case object PortConfiguration {
  def apply (node: JsonNode): PortConfiguration = {
    val ports = node.get ("ports") match {
      case null => Nil
      case portsNode: ArrayNode => portsNode.elements.asScala.map { item =>
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
    }
    val portHandlers = node.get ("portHandlers") match {
      case null => Nil
      case handlersNode: ArrayNode => handlersNode.elements.asScala.map { item =>
        makePortHandler (item.asInstanceOf[ObjectNode])
      }.toList
    }
    new PortConfiguration (
      ports,
      portHandlers
    )
  }

  private def makePortHandler (item: ObjectNode): PortHandler = {
    val className = item.get ("class").asText ()
    val cls = Class.forName (className)
    val params = makeParams (item.get ("params").asInstanceOf[ArrayNode])
    val ctor = findConstructor (cls, params)
    ctor.newInstance (params.toArray.asInstanceOf[Array[_ <: Object]]:_*).asInstanceOf[PortHandler]
  }

  private def makeParams (item: ArrayNode): Seq[Any] = {
    if (item == null) {return Nil}
    val params = item.elements ().asScala
    params.map {param =>
      param.isIntegralNumber match {
        case true => param.asInt
        case false => param.asText match {
          case v if v.startsWith ("0x") => textToInt (v)
          case v => v
        }
      }
    }.toSeq
  }

  private def findConstructor (cls: Class[_], params: Seq[Any]): Constructor[_] = {
    val availableTypeList = params.map {
      case c: Integer => Integer.TYPE
      case c: String => classOf[String]
      case _ => TEST_DRIVE_ME
    }
    cls.getConstructor (availableTypeList.toArray:_*)
  }
}

case class PortConfiguration (
  ports: Seq[(String, Port)],
  portHandlers: Seq[PortHandler]
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
  private val (minPort, maxPort) = findMinMax (portsByName.values.map {_.address})
  private val portsByAddress: Map[Int, Seq[Port]] = groupPortsByAddress (portsByName.values)
  private val handlersByName: Map[String, PortHandler] = extractHandlersByName (configs)
  private val handlersByPortName: Map[String, Seq[PortHandler]] = groupHandlersByPortName (handlersByName.values)

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
    if (address < minPort) {return}
    if (address > maxPort) {return}
    val portsForAddress = portsByAddress.getOrElse (address, Nil)
    val portsAffected = portsForAddress.filter {_.affectedByChange (oldValue, newValue)}
    portsAffected.foreach {port =>
      val handlersAffected = handlersByPortName.getOrElse (port.name, Nil)
      handlersAffected.foreach {handler =>
        val mask = (1 << port.bitLength) - 1
        val rshift = port.lowBit
        val ov = (oldValue >> rshift) & mask
        val nv = (newValue >> rshift) & mask
        handler.acceptChange (port.name, ov, nv)
      }
    }
  }

  private def extractPortsByName (configs: Seq[PortConfiguration]): Map[String, Port] = {
    configs.foldLeft (List[(String, Port)] ()) {(soFar, config) => soFar ++ config.ports}.toMap
  }

  private def findMinMax (numbers: Iterable[Int]): (Int, Int) = {
    numbers.foldLeft ((Integer.MAX_VALUE, Integer.MIN_VALUE)) {(soFar, number) =>
      val (curMin, curMax) = soFar
      if (number < curMin) {
        (number, curMax)
      }
      else if (number > curMax) {
        (curMin, number)
      }
      else {
        soFar
      }
    }
  }

  private def groupPortsByAddress (ports: Iterable[Port]): Map[Int, Seq[Port]] = {
    ports.foldLeft (Map[Int, List[Port]] ()) {(soFar, port) =>
      soFar.get (port.address) match {
        case Some (seq) => soFar + (port.address -> (port :: seq))
        case None => soFar + (port.address -> (port :: Nil))
      }
    }
  }

  private def extractHandlersByName (configs: Iterable[PortConfiguration]): Map[String, PortHandler] = {
    val handlers = configs.foldLeft (List[PortHandler] ()) {(soFar, config) =>
      soFar ++ config.portHandlers
    }
    handlers.map {handler =>
      handler.initialize (cpu)
      validatePortHandler (handler, portsByName)
      if (classOf[TickSink].isAssignableFrom (handler.getClass)) {
        cpu.engine.addTickSink (handler.asInstanceOf[TickSink])
      }
      (handler.name, handler)
    }.toMap
  }

  private def groupHandlersByPortName (handlers: Iterable[PortHandler]): Map[String, Seq[PortHandler]] = {
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
