package simulino.cpu.arch.avr.ATmega

import org.scalatest.path
import simulino.cpu.arch.avr.SetFlags
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

    describe ("when experimenting with fullCarry") {
      val b7T = UnsignedByte (0x80)
      val b7F = UnsignedByte (0x00)

      it ("says (F, F, F) -> F") {
        assert (subject.fullCarry (b7F, b7F, b7F) === false);
      }

      it ("says (F, F, T) -> T") {
        assert (subject.fullCarry (b7F, b7F, b7T) === true);
      }

      it ("says (F, T, F) -> T") {
        assert (subject.fullCarry (b7F, b7T, b7F) === true);
      }

      it ("says (F, T, T) -> F") {
        assert (subject.fullCarry (b7F, b7T, b7T) === true);
      }

      it ("says (T, F, F) -> F") {
        assert (subject.fullCarry (b7T, b7F, b7F) === false);
      }

      it ("says (T, F, T) -> F") {
        assert (subject.fullCarry (b7T, b7F, b7T) === false);
      }

      it ("says (T, T, F) -> F") {
        assert (subject.fullCarry (b7T, b7T, b7F) === false);
      }

      it ("says (T, T, T) -> F") {
        assert (subject.fullCarry (b7T, b7T, b7T) === true);
      }
    }

    describe ("when experimenting with negative") {
      it ("a negative number produces true") {
        assert (subject.negative (UnsignedByte (0x80)) === true)
      }

      it ("a positive number produces false") {
        assert (subject.negative (UnsignedByte (0x7F)) === false)
      }

      it ("zero produces false") {
        assert (subject.negative (UnsignedByte (0x00)) === false)
      }
    }

    describe ("when experimenting with zero") {
      it ("a negative number produces false") {
        assert (subject.zero (UnsignedByte (0x80)) === false)
      }

      it ("a positive number produces false") {
        assert (subject.zero (UnsignedByte (0x7F)) === false)
      }

      it ("zero produces true") {
        assert (subject.zero (UnsignedByte (0x00)) === true)
      }
    }

    describe ("when experimenting with overflow") {
      val b7T = UnsignedByte (0x80)
      val b7F = UnsignedByte (0x00)

      it ("says (F, F, F) -> F") {
        assert (subject.overflow (b7F, b7F, b7F) === false);
      }

      it ("says (F, F, T) -> T") {
        assert (subject.overflow (b7F, b7F, b7T) === false);
      }

      it ("says (F, T, F) -> T") {
        assert (subject.overflow (b7F, b7T, b7F) === false);
      }

      it ("says (F, T, T) -> F") {
        assert (subject.overflow (b7F, b7T, b7T) === true);
      }

      it ("says (T, F, F) -> F") {
        assert (subject.overflow (b7T, b7F, b7F) === true);
      }

      it ("says (T, F, T) -> F") {
        assert (subject.overflow (b7T, b7F, b7T) === false);
      }

      it ("says (T, T, F) -> F") {
        assert (subject.overflow (b7T, b7T, b7F) === false);
      }

      it ("says (T, T, T) -> F") {
        assert (subject.overflow (b7T, b7T, b7T) === false);
      }
    }

    describe ("when experimenting with sign") {
      val b7T = UnsignedByte (0x80)
      val b7F = UnsignedByte (0x00)

      it ("negative false and overflow false produces sign false") {
        assert (subject.sign (b7F, b7F, b7F) === false)
      }

      it ("negative false and overflow true produces sign true") {
        assert (subject.sign (b7T, b7F, b7F) === true)
      }

      it ("negative true and overflow false produces sign true") {
        assert (subject.sign (b7T, b7T, b7T) === true)
      }

      it ("negative true and overflow true produces sign false") {
        assert (subject.sign (b7F, b7T, b7T) === false)
      }
    }

    describe ("when directed to do a default build where everything but S and Z is true") {
      val result = subject.builder (
        UnsignedByte (0x00),
        UnsignedByte (0x88),
        UnsignedByte (0x88)
      ).make ()

      it ("makes everything but S and Z true") {
        assert (result === SetFlags (None, None, Some (true), Some (false), Some (true), Some (true),
          Some (false), Some (true)))
      }
    }

    describe ("when directed to do a default build where at least Z is true") {
      val result = subject.builder (
        UnsignedByte (0x00),
        UnsignedByte (0x00),
        UnsignedByte (0x00)
      ).make ()

      it ("makes Z true") {
        assert (result === SetFlags (None, None, Some (false), Some (false), Some (false), Some (false),
          Some (true), Some (false)))
      }
    }

    describe ("when directed to do a default build where everything is false") {
      val result = subject.builder (
        UnsignedByte (0x00),
        UnsignedByte (0x00),
        UnsignedByte (0x01)
      ).make ()

      it ("makes everything false") {
        assert (result === SetFlags (None, None, Some (false), Some (false), Some (false), Some (false),
          Some (false), Some (false)))
      }
    }

    describe ("when directed to set everything to None") {
      val result = subject.builder (UnsignedByte (0x00), UnsignedByte (0x00), UnsignedByte (0x00))
        .halfCarry (None)
        .sign (None)
        .overflow (None)
        .negative (None)
        .zero (None)
        .carry (None)
        .make ()

      it ("makes everything None") {
        assert (result === SetFlags ())
      }
    }

    describe ("when directed to set everything to true") {
      val result = subject.builder (UnsignedByte (0x00), UnsignedByte (0x00), UnsignedByte (0x00))
        .halfCarry (Some (true))
        .sign (Some (true))
        .overflow (Some (true))
        .negative (Some (true))
        .zero (Some (true))
        .carry (Some (true))
        .make ()

      it ("makes everything true") {
        assert (result === SetFlags (None, None, Some (true), Some (true), Some (true), Some (true),
          Some (true), Some (true)))
      }
    }
  }
}
