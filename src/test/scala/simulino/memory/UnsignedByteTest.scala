package simulino.memory

import org.scalatest.path

//noinspection ComparingUnrelatedTypes
class UnsignedByteTest extends path.FunSpec {
  describe ("An UnsignedByte") {
    describe ("when created with a value") {
      val ubyte = UnsignedByte(0x42)
      it ("should retain that value") {
        assert (ubyte.value === 0x42)
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
      it ("becomes a negative integer") {
        assert (integer === -66)
      }
    }
    describe ("can convert to Byte") {
      val ubyten = UnsignedByte (0xBE)
      val byten: Byte = ubyten
      assert (byten === -66)
      
      val ubytep = UnsignedByte (0x42)
      val bytep: Byte = ubytep
      assert (bytep === 66)
    }
    describe ("works with Ints in") {
      val ubyte = UnsignedByte (0x42)
      describe ("addition") {
	      it ("without half-carry or overflow") {
	        val result = ubyte + 0x22
	        assert (result.value === 0x64)
	        assert (result.halfCarry === false)
	        assert (result.overflow === false)
	        assert (result.zero === false)
	        assert (result.negative === false)
	        assert (result.carry === false)
	      }
	      it ("with half-carry but no overflow") {
	        val result = ubyte + 0x0E
	        assert (result.value === 0x50)
	        assert (result.halfCarry === true)
	        assert (result.overflow === false)
	        assert (result.zero === false)
	        assert (result.negative === false)
	        assert (result.carry === false)
	      }
	      it ("resulting in positive overflow") {
	        val result = ubyte + 0x3E
	        assert (result.value === 0x80)
	        assert (result.overflow === true)
	        assert (result.zero === false)
	        assert (result.negative === true)
	        assert (result.carry === false)
	      }
	      it ("resulting in negative overflow") {
	        val nubyte = new UnsignedByte (0xBE)
	        val result = nubyte + 0xC1
	        assert (result.value === 0x7F)
	        assert (result.overflow === true)
	        assert (result.zero === false)
	        assert (result.negative === false)
	        assert (result.carry === true)
	      }
	      it ("resulting in zero") {
	    	val result = ubyte + 0xBE
	    	assert (result.value === 0x00)
	        assert (result.overflow === false)
	    	assert (result.zero === true)
	        assert (result.negative === false)
	        assert (result.carry === true)
	      }
	      it ("of positive and negative") {
	    	val result = ubyte + 0xBD
	    	assert (result.value === 0xFF)
	        assert (result.overflow === false)
	    	assert (result.zero === false)
	        assert (result.negative === true)
	        assert (result.carry === false)
	      }
      }
      describe ("subtraction") {
        describe ("when figuring 0xAA - 0xCC") {
          val diff = UnsignedByte (0xAA) - new UnsignedByte (0xCC)
          it ("should compute 0xDE as the difference") {
            assert (diff.value === 0xDE)
          }
          it ("should set half-carry and carry but not overflow") {
            assert (diff.halfCarry)
            assert (diff.carry)
            assert (!diff.overflow)
          }
        }
        describe ("when figuring 0x56 - 0x65") {
          val diff = UnsignedByte (0x56) - new UnsignedByte (0x65)
          it ("should compute 0xF1 as the difference") {
            assert (diff.value === 0xF1)
          }
          it ("should set carry and overflow but not half-carry") {
            assert (diff.carry)
            assert (diff.overflow)
            assert (!diff.halfCarry)
          }
        }
        describe ("when figuring 0x42 - 0x42") {
          val diff = UnsignedByte (0x42) - new UnsignedByte (0x42)
          it ("should compute 0x00 as the difference") {
            assert (diff.value === 0x00)
          }
          it ("should clear half-carry, carry, and overflow") {
            assert (!diff.halfCarry)
            assert (!diff.carry)
            assert (!diff.overflow)
          }
        }
      }
      describe ("xor") {
	      it ("to set all bits") {
	        val result = ubyte ^ 0xBD
	        assert (result.value === 0xFF)
	        assert (result.zero === false)
	        assert (result.negative === true)
	      }
	      it ("to clear all bits") {
	        val result = ubyte ^ 0x42
	        assert (result.value === 0x00)
	        assert (result.zero === true)
	        assert (result.negative === false)
	      }
      }
    }
    it ("should convert itself properly to a String") {
      assert (UnsignedByte (0x42).toString () === "42")
    }
  }
}