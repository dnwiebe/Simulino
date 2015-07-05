package simulino.memory

import org.scalatest.path

//noinspection ComparingUnrelatedTypes
class UnsignedByteTest extends path.FunSpec {
  describe ("An UnsignedByte") {
    describe ("when created with a positive value") {
      val ubyte = UnsignedByte(0x42)
      it ("should retain that value") {
        assert (ubyte.value === 0x42)
      }
      it ("returns a positive signedValue") {
        assert (ubyte.signedValue === 0x42)
      }
      it ("claims to be equal to an equivalent but different UnsignedByte") {
        assert ((ubyte == UnsignedByte (0x42)) === true)
      }
      it ("claims not to be equal to a differently-valued UnsignedByte") {
        assert ((ubyte == UnsignedByte (0x43)) === false)
      }
      it ("claims to be equal to an Int with the same value") {
        assert ((ubyte == 0x42) === true)
      }
      it ("claims not to be equal to an Int with a different value") {
        assert ((ubyte == 0x43) === false)
      }
    }
    describe ("when created with a value having bit 7 set"){
      val ubyte = UnsignedByte (0xC2)
      it ("returns a nonnegative value") {
        assert (ubyte.value === 0xC2)
      }
      it ("returns a negative signedValue") {
        assert (ubyte.signedValue === 0xFFFFFFC2)
      }
    }
    describe ("when created with an alternating sequence of bits") {
      describe ("starting with a 1") {
        val ubyte = UnsignedByte (0xAA)
        it ("bits are individually addressable") {
          assert ((ubyte bit 0) === false)
          assert ((ubyte bit 1) === true)
          assert ((ubyte bit 2) === false)
          assert ((ubyte bit 3) === true)
          assert ((ubyte bit 4) === false)
          assert ((ubyte bit 5) === true)
          assert ((ubyte bit 6) === false)
          assert ((ubyte bit 7) === true)
        }
      }
      describe ("starting with a 0") {
        val ubyte = UnsignedByte (0x55)
        it ("bits are individually addressable") {
          assert ((ubyte bit 0) === true)
          assert ((ubyte bit 1) === false)
          assert ((ubyte bit 2) === true)
          assert ((ubyte bit 3) === false)
          assert ((ubyte bit 4) === true)
          assert ((ubyte bit 5) === false)
          assert ((ubyte bit 6) === true)
          assert ((ubyte bit 7) === false)
        }
      }
    }
    describe ("when created with a three-bit repeating pattern") {
      val ubyte = 0x49
      it ("handles a two-bit left shift properly") {
        assert ((ubyte << 2) === 0x124)
      }
      it ("handles a three-bit left shift properly") {
        assert ((ubyte << 3) === 0x248)
      }

      it ("handles a two-bit right shift properly") {
        assert ((ubyte >> 2) === 0x12)
      }
      it ("handles a three-bit right shift properly") {
        assert ((ubyte >> 3) === 0x09)
      }
    }
    describe ("when assigned a value less than 0x80") {
      val ubyte: UnsignedByte = 0x7F
      it ("should show that value") {
        assert (ubyte == 0x7F)
      }
    }
    describe ("when assigned a value of 0x80") {
      val ubyte: UnsignedByte = 0x80
      it ("should show that value") {
        assert (ubyte == 0x80)
      }
    }
    describe ("when assigned a value greater than 0xFF") {
      it ("should complain") {
        try {
          val ubyte: UnsignedByte = 0x100
          fail ()
        }
        catch {
          case e: IllegalArgumentException => { assert (e.getMessage === "UnsignedByte is restricted to values between 0 and 255, not 256") }
        }
      }
    }
    describe ("when assigned a negative value and converted to an integer") {
      val integer: Int = new UnsignedByte (0xBE)
      it ("does not become a negative integer") {
        assert (integer === 0xBE)
      }
    }
    describe ("when converted to a (signed) Byte") {
      val ubyten = UnsignedByte (0xBE)
      val byten: Byte = ubyten

      it ("becomes negative if greater than 0x7F") {
        assert (byten === -66)
      }
      
      val ubytep = UnsignedByte (0x42)
      val bytep: Byte = ubytep

      it ("stays positive if 0x7F or less") {
        assert (bytep === 66)
      }
    }
    describe ("works with Ints in") {
      val ubyte = UnsignedByte (0x42)
      it ("addition") {
        assert ((ubyte + 0x82).value === 0xC4)
      }
      describe ("subtraction") {
        it ("when figuring 0xAA - 0xCC = 0xDE") {
          assert ((UnsignedByte (0xAA) - new UnsignedByte (0xCC)).value === 0xDE)
        }
        it ("when figuring 0x56 - 0x65 = 0xF1") {
          assert ((UnsignedByte (0x56) - new UnsignedByte (0x65)).value === 0xF1)
        }
        it ("when figuring 0x42 - 0x42 = 0") {
          assert ((UnsignedByte (0x42) - new UnsignedByte (0x42)).value === 0x00)
        }
      }
      describe ("xor") {
	      it ("to set all bits") {
	        assert ((ubyte ^ 0xBD).value === 0xFF)
	      }
	      it ("to clear all bits") {
	        assert ((ubyte ^ 0x42).value === 0x00)
	      }
      }
    }
    it ("should convert itself properly to a String") {
      assert (UnsignedByte (0x42).toString () === "42")
    }
    describe ("prepared for business-equals testing") {
      val a = UnsignedByte (0x42)
      val b = UnsignedByte (0x42)
      val c = UnsignedByte (0x43)
      it ("it behaves properly") {
        assert (a.equals (a) === true)
        assert (a.equals (b) === true)
        assert (a.equals (c) === false)
        assert (a.equals (null) === false)
        assert (a.equals ("blah") === false)
      }
    }
  }
}