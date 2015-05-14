package simulino.cpu

/**
 * Created by dnwiebe on 5/13/15.
 */

import org.scalatest.path
import simulino.cpu.Implicits._

class InstructionTest extends path.FunSpec {
  describe ("Regarding bit selectors") {
    it ("bit 7 of 0x00 is false") {
      assert ((0x00 bit 7) === false)
    }

    it ("bit 7 of 0xFF is true") {
      assert ((0xFF bit 7) === true)
    }

    it ("bit 4 of 0x10 is true") {
      assert ((0x10 bit 4) === true)
    }

    it ("bit 30 of 0xBFFFFFFF is false") {
      assert ((0xBFFFFFFF bit 30) === false)
    }
  }

  describe ("Regarding boolean arithmetic") {

    it ("false ^^ false => false") {
      assert ((false ^^ false) === false)
    }

    it ("false ^^ true => true") {
      assert ((false ^^ true) === true)
    }

    it ("true ^^ false => true") {
      assert ((true ^^ false) === true)
    }

    it ("true ^^ true => false") {
      assert ((true ^^ true) === false)
    }
  }
}
