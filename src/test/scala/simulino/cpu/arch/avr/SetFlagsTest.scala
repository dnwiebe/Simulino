package simulino.cpu.arch.avr

import org.scalatest.path

/**
 * Created by dnwiebe on 5/21/15.
 */
class SetFlagsTest extends path.FunSpec {
  describe ("A SetFlags(________)") {
    val subject = SetFlags ()

    it ("specifies nothing") {
      assert (subject.I === None)
      assert (subject.T === None)
      assert (subject.H === None)
      assert (subject.S === None)
      assert (subject.V === None)
      assert (subject.N === None)
      assert (subject.Z === None)
      assert (subject.C === None)
    }

    it ("has the proper parameters") {
      assert (subject.mask === 0x00)
      assert (subject.pattern === 0x00)
    }

    it ("shows the right toString") {
      assert (subject.toString === "SetFlags(________)")
    }
  }

  describe ("A SetFlags(ItHsVnZc)") {
    val subject = SetFlags (I = Some(true), T = Some (false), H = Some (true), S = Some (false),
      V = Some (true), N = Some (false), Z = Some (true), C = Some (false))

    it ("specifies everything") {
      assert (subject.I === Some (true))
      assert (subject.T === Some (false))
      assert (subject.H === Some (true))
      assert (subject.S === Some (false))
      assert (subject.V === Some (true))
      assert (subject.N === Some (false))
      assert (subject.Z === Some (true))
      assert (subject.C === Some (false))
    }

    it ("has the proper parameters") {
      assert (subject.mask === 0xFF)
      assert (subject.pattern === 0xAA)
    }

    it ("shows the right toString") {
      assert (subject.toString === "SetFlags(ItHsVnZc)")
    }
  }

  describe ("A SetFlags(iThSvNzC)") {
    val subject = SetFlags (I = Some(false), T = Some (true), H = Some (false), S = Some (true),
      V = Some (false), N = Some (true), Z = Some (false), C = Some (true))

    it ("specifies everything") {
      assert (subject.I === Some (false))
      assert (subject.T === Some (true))
      assert (subject.H === Some (false))
      assert (subject.S === Some (true))
      assert (subject.V === Some (false))
      assert (subject.N === Some (true))
      assert (subject.Z === Some (false))
      assert (subject.C === Some (true))
    }

    it ("has the proper parameters") {
      assert (subject.mask === 0xFF)
      assert (subject.pattern === 0x55)
    }

    it ("shows the right toString") {
      assert (subject.toString === "SetFlags(iThSvNzC)")
    }
  }
}
