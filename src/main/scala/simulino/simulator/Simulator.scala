package simulino.simulator

import java.io.{InputStreamReader, InputStream, Reader}

import simulino.cpu.Cpu
import simulino.engine.{ScheduledEvent, Event, Engine, Subscriber}
import simulino.hex.HexLoader
import simulino.memory.{Memory, UnsignedByte}
import simulino.utils.Utils._

/**
 * Created by dnwiebe on 5/10/15.
 */
class Simulator (configuration: SimulatorConfiguration) {
  var engine = new Engine ()
  val programMemory = new Memory (configuration.memory.programSize)
  val cpu = makeCpu (configuration.cpu)

  def id = System.identityHashCode(this)

  def loadHex (hex: Reader): Unit = {
    val loader = new HexLoader ()
    loader.load (hex, programMemory)
  }

  def loadHex (hex: InputStream): Unit = {
    loadHex (new InputStreamReader (hex))
  }

  def addSubscriber (subscriber: Subscriber): Unit = {
    engine.addSubscriber (subscriber)
  }

  def schedule (event: ScheduledEvent): Unit = {
    engine.schedule (event)
  }

  def runForSeconds (seconds: Double): Unit = {
    var ticks = (configuration.cpu.clockSpeed * seconds).toLong
    while (ticks > 0) {
      engine.tick ()
      ticks -= 1
    }
  }

  def dumpProgramMemory (offset: Int, length: Int): Array[UnsignedByte] = {
    programMemory.getData (offset, length)
  }

  def dumpDynamicMemory (offset: Int, length: Int): Array[UnsignedByte] = {
    (0 until length).map {i => UnsignedByte (0)}.toArray
  }

  def dumpPersistentMemory (offset: Int, length: Int): Array[UnsignedByte] = {
    (0 until length).map {i => UnsignedByte (0)}.toArray
  }

  private def makeCpu (config: CpuConfiguration): Cpu = {
    val ctor = config.cls.getConstructor (classOf[Engine], classOf[CpuConfiguration])
    ctor.newInstance (engine, config)
  }
}
