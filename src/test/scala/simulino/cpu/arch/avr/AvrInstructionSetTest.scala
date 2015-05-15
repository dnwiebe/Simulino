package simulino.cpu.arch.avr

import org.scalatest.path
import simulino.cpu.arch.avr.ATmega.{RJMP, NOP}
import simulino.utils.TestUtils._

/**
 * Created by dnwiebe on 5/15/15.
 */
class AvrInstructionSetTest extends path.FunSpec {
  describe ("Given an AvrInstructionSet") {
    val subject = new AvrInstructionSet ()
    
    describe ("In sesquidecile 0") {
      it ("NOP is recognized") {
        val instruction = subject (unsignedBytes (0x00, 0x00)).get
        assert (instruction.getClass === classOf[NOP])
      }
    }

    describe ("In sesquidecile C") {
      it ("RJMP is recognized") {
        val instruction = subject (unsignedBytes (0xC1, 0x23)).get
        assert (instruction.getClass === classOf[RJMP])
        assert (instruction.asInstanceOf[RJMP].k === 0x123)
      }
    }
  }
}
