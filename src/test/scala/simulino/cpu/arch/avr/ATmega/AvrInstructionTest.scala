package simulino.cpu.arch.avr.ATmega

import org.scalatest.path
import simulino.memory.UnsignedByte

/**
 * Created by dnwiebe on 7/3/15.
 */
class AvrInstructionTest extends path.FunSpec {
  describe ("A subject that extends AvrInstructionUtils") {
    val subject = new AvrInstructionUtils () {}

    describe ("when experimenting with halfCarry") {
      val b3T = UnsignedByte (0x08)
      val b3F = UnsignedByte (0x00)

      it ("says (F, F, F) -> F") {
        assert (subject.halfCarry (b3F, b3F, b3F) === false);
      }

      it ("says (F, F, T) -> T") {
        assert (subject.halfCarry (b3F, b3F, b3T) === true);
      }

      it ("says (F, T, F) -> T") {
        assert (subject.halfCarry (b3F, b3T, b3F) === true);
      }

      it ("says (F, T, T) -> F") {
        assert (subject.halfCarry (b3F, b3T, b3T) === true);
      }

      it ("says (T, F, F) -> F") {
        assert (subject.halfCarry (b3T, b3F, b3F) === false);
      }

      it ("says (T, F, T) -> F") {
        assert (subject.halfCarry (b3T, b3F, b3T) === false);
      }

      it ("says (T, T, F) -> F") {
        assert (subject.halfCarry (b3T, b3T, b3F) === false);
      }

      it ("says (T, T, T) -> F") {
        assert (subject.halfCarry (b3T, b3T, b3T) === true);
      }
    }
  }
}
