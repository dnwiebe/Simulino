package simulino.cpu.arch.avr.peripheral

import simulino.cpu.arch.avr.PortHandler

/**
 * Created by dnwiebe on 6/10/15.
 */
class PinPortHandler (suffix: String) extends PortHandler {
  override val name = s"Port ${suffix}"
  override val portNames = makePortNames (suffix)

  private def makePortNames (suffix: String): List[String] = {
    val idxs = (0 until 7).toList
    idxs.map {i => s"PIN${suffix}${i}"} ++ idxs.map {i => s"DD${suffix}${i}"} ++ idxs.map {i => s"PORT${suffix}${i}"}
  }
}
