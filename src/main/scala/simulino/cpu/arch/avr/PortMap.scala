package simulino.cpu.arch.avr

import simulino.engine.Engine
import simulino.memory.{Memory, UnsignedByte}
import simulino.utils.Utils._

/**
 * Created by dnwiebe on 5/23/15.
 */

trait Subscriber {
  def changeNotification ()
}

class PortMap (cpu: AvrCpu, memory: Memory) {
  def outgoingChangeNotification (address: Int, value: UnsignedByte): Unit = {
    TEST_DRIVE_ME
  }

  def incomingChangeNotification (portName: String, lowBit: Int, bitLength: Int, value: Int): Unit = {
    TEST_DRIVE_ME
  }

  def addSubscriber (subscriber: Subscriber, portName: String): Unit = {
    TEST_DRIVE_ME
  }
}
