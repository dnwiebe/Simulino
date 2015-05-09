package simulino.memory

import org.scalatest.path

//noinspection ComparingUnrelatedTypes
class UnsignedShortTest extends path.FunSpec {
  describe ("An UnsignedShort") {
    it ("should be automatically converted to an Int when necessary") {
      val integer: Int = new UnsignedShort (0x4242)
      assert (integer === 0x4242)
    }
    describe ("when created with a value") {
      val ushort = new UnsignedShort(0x4242)
      it ("should retain that value") {
        assert (ushort.value === 0x4242)
      }
    }
    describe ("when created with two UnsignedBytes") {
      val ushort = new UnsignedShort (new UnsignedByte (0x42), new UnsignedByte (0x24))
      it ("should assimilate them in the right order") {
        assert (ushort.value === 0x4224)
      }
    }
    describe ("when assigned a value less than 0x8000") {
      val ushort: UnsignedShort = 0x7FFF
      it ("should show that value") {
        assert (ushort == 0x7FFF)
      }
    }
    describe ("when assigned a value of 0x8000") {
      val ushort: UnsignedShort = 0x8000
      it ("should show that value") {
        assert (ushort == 0x8000)
      }
    }
    describe ("when assigned a value greater than 0xFFFF") {
      it ("should complain") {
        try {
          val ushort: UnsignedShort = 0x10000
          fail ()
        }
        catch {
          case e: IllegalArgumentException => { assert (e.getMessage === "UnsignedShort is restricted to values between 0x0000 and 0xFFFF, not 0x10000") }
        }
      }
    }
    describe ("when assigned a value that's negative as a short") {
      val ushort: UnsignedShort = 0xFFFF9518
      it ("should clip off the sign extension") {
        assert (ushort == 0x9518)
      }
    }
    it ("should convert itself into a String") {
      assert (new UnsignedShort (0x1234).toString () === "1234")
    }
    it ("should pull high byte") {
      assert (new UnsignedShort (0x2442).highByte.value === 0x24)
    }
    it ("should pull low byte") {
      assert (new UnsignedShort (0x2442).lowByte.value === 0x42)
    }
  }
}