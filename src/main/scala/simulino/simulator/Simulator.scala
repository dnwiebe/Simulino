package simulino.simulator

import java.io.{InputStreamReader, InputStream, Reader}

import simulino.cpu.Cpu
import simulino.engine.{ScheduledEvent, Event, Engine, Subscriber}
import simulino.hex.HexLoader
import simulino.memory.{Memory, UnsignedByte}
import simulino.simulator.peripheral.PinSampler
import simulino.utils.Utils._

/**
 * Created by dnwiebe on 5/10/15.
 */
class Simulator (configuration: SimulatorConfiguration) {
  var engine = new Engine ()
  val cpu = prepareCpu (configuration.cpu)

  def id = System.identityHashCode(this)

  def loadHex (hex: Reader): Unit = {
    val loader = new HexLoader ()
    loader.load (hex, cpu.programMemory)
    engine.schedule (cpu.nextInstruction(), engine.currentTick)
  }

  def loadHex (hex: InputStream): Unit = {
    loadHex (new InputStreamReader (hex))
  }

  def addSubscriber (subscriber: Subscriber): Unit = {
    engine.addSubscriber (subscriber)
  }

  def addPinSampler (sampler: PinSampler): Unit = {
    cpu.addPinSampler (sampler)
  }

  def schedule (event: ScheduledEvent): Unit = {
    engine.schedule (event)
  }

  def runForTicks (ticks: Long): Unit = {
    var mutableTicks = ticks;
    while (mutableTicks > 0) {
      engine.tick ()
      mutableTicks -= 1L
    }
  }

  def runForSeconds (seconds: Double): Unit = {
    val ticks = (configuration.cpu.clockSpeed * seconds).toLong
    runForTicks (ticks)
  }

  def dumpProgramMemory (offset: Int, length: Int): Array[UnsignedByte] = {
    cpu.programMemory.getData (offset, length)
  }

  def dumpDynamicMemory (offset: Int, length: Int): Array[UnsignedByte] = {
    (0 until length).map {i => UnsignedByte (0)}.toArray
  }

  def dumpPersistentMemory (offset: Int, length: Int): Array[UnsignedByte] = {
    (0 until length).map {i => UnsignedByte (0)}.toArray
  }

  private def prepareCpu (config: CpuConfiguration): Cpu = {
    val programMemory = new Memory (configuration.memory.programSize)
    val ctor = config.cls.getConstructor (classOf[Engine], classOf[Memory], classOf[CpuConfiguration])
    val cpu = ctor.newInstance (engine, programMemory, config)
    engine.addSubscriber (cpu)
    cpu
  }
}
