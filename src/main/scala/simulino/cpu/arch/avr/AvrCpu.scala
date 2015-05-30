package simulino.cpu.arch.avr

import com.fasterxml.jackson.databind.JsonNode
import com.fasterxml.jackson.databind.node.ArrayNode
import simulino.cpu.arch.avr.ATmega.{Flag, SetFlags, SetMemory}
import simulino.cpu._
import simulino.engine.Engine
import simulino.memory.{Span, Memory, UnsignedByte}
import simulino.simulator.CpuConfiguration
import simulino.utils.Utils._
import scala.collection.JavaConverters._

/**
 * Created by dnwiebe on 5/13/15.
 */

object RegisterNames {
  // TODO: These should all be configurable from the JSON file.
  val SPH = 0x5E
  val SPL = 0x5D
  val XL = 0x1A
  val XH = 0x1B
  val YL = 0x1C
  val YH = 0x1D
  val ZL = 0x1E
  val ZH = 0x1F
  val RAMPX = 0x59
  val RAMPY = 0x5A
  val RAMPZ = 0x5B
  val SREG = 0x5F

  val Xfull = (RAMPX, XH, XL)
  val Yfull = (RAMPY, YH, YL)
  val Zfull = (RAMPZ, ZH, ZL)

  def getExtended (cpu: AvrCpu, registers: (Int, Int, Int)): Int = {
    ((cpu.register (registers._1).value & 0xFF) << 16) |
      ((cpu.register (registers._2).value & 0xFF) << 8) |
      (cpu.register (registers._3).value & 0xFF)
  }
  def setExtended (registers: (Int, Int, Int), value: Int): List[CpuChange] = {
    List (
      SetMemory (registers._1, (value >> 16) & 0xFF),
      SetMemory (registers._2, (value >> 8) & 0xFF),
      SetMemory (registers._3, value & 0xFF)
    )
  }
}

class AvrCpu (val engine: Engine, val programMemory: Memory, val config: CpuConfiguration) extends Cpu {
  import RegisterNames._

  val dataMemory = createMemory (config.classSpecific)
  val instructionSet = new AvrInstructionSet ()
  val portMap = new PortMap (this, portMapConfigurations (config.classSpecific))
  val interruptVectors = extractInterruptVectors (config.classSpecific)

  def register (address: Int): UnsignedByte = dataMemory.getData (address, 1)(0)
  def setMemory (address: Int, value: UnsignedByte): Unit = {dataMemory.update (address, value)}

  override def sp: Int = ((register (SPH).value << 8) | register (SPL).value) & 0xFFFF
  protected override def sp_= (value: Int): Unit = {
    setMemory (SPH, (value >> 8) & 0xFF)
    setMemory (SPL, value & 0xFF)
  }

  override def handleCpuChange (change: CpuChange): Unit = {
    change match {
      case c: SetMemory => handleSetMemory (c.address, c.value)
      case c: SetFlags => handleSetFlags (c)
      case c: PushIp => handlePushIp ()
      case c: Push => handlePush (c)
      case x => super.handleCpuChange (x)
    }
  }

  def flag (name: Flag): Boolean = {
    val octet = register (SREG).value
    val idx = 7 - name.ordinal ()
    val shifted = octet >> idx
    (shifted & 0x01) == 1
  }

  def raiseInterrupt (name: String): Unit = {
    val sreg = register (SREG).value
    if ((sreg & 0x80) == 0) {return}
    val vector = interruptVectors(name)
    val tick = engine.currentTick + 5
    engine.schedule (PushIp (), tick)
    engine.schedule (SetIp (vector), tick)
    engine.schedule (SetFlags (I = Some (false)), tick)
  }

  private def handleSetMemory (address: Int, value: UnsignedByte): Unit = {
    setMemory (address, value)
  }

  private def handleSetFlags (c: SetFlags): Unit = {
    val original = register (SREG).value
    val withSets = original | (c.mask & c.pattern)
    val withSetsAndClears = withSets & (~c.mask | c.pattern)
    setMemory (SREG, withSetsAndClears)
  }

  private def handlePushIp (): Unit = {
    val nextIp = ip + 2
    val firstByte = (nextIp >> 16) & 0xFF
    val secondByte = (nextIp >> 8) & 0xFF
    val thirdByte = nextIp & 0xFF
    val data = Array(UnsignedByte (firstByte), UnsignedByte (secondByte), UnsignedByte (thirdByte))
    dataMemory.addSpan (Span (sp - 2, data))
    sp = sp - 3
  }

  private def handlePush (c: Push): Unit = {
    dataMemory.update (sp, c.value)
    sp = sp - 1
  }

  private def portMapConfigurations (classSpecific: JsonNode): List[PortConfiguration] = {
    if (classSpecific == null) {return Nil}
    val configNode = classSpecific.get ("portMap")
    if (configNode == null) Nil else List (PortConfiguration (configNode))
  }

  private def extractInterruptVectors (classSpecific: JsonNode): Map[String, Int] = {
    if (classSpecific == null) {return Map ()}
    classSpecific.get ("interruptVectors").asInstanceOf[ArrayNode].elements ().asScala.map {node =>
      (node.get ("name").asText, hexOrDec (node.get ("address")))
    }.toMap
  }

  private def createMemory (classSpecific: JsonNode): Memory = {
    if (classSpecific == null) {return new Memory (0)}
    val memoryNode = classSpecific.get ("memory")
    val internal = hexOrDec (memoryNode.get ("internal"))
    val sram = hexOrDec (memoryNode.get ("sram"))
    new Memory (internal + sram)
  }

  // for testing only
  def setSpForTest (value: Int): Unit = {sp = value}
}
