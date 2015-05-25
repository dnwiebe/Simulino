package simulino.utils

import org.scalatest.path

class UtilsTest extends path.FunSpec {
	describe ("toHex") {
	  describe ("when given a short number and asked to format it long") {
	    val string = Utils.toHex (66, 5)
	    it ("does so") {
	      assert (string === "00042")
	    }
	  }
	  describe ("when given a negative number and asked to format it long") {
	    val string = Utils.toHex (-66, 5)
	    it ("does so") {
	      assert (string === "FFFBE")
	    }
	  }
	  describe ("when given a long number and asked to format it short") {
	    val string = Utils.toHex (0x12345, 2)
	    it ("formats it long instead") {
	      assert (string === "12345")
	    }
	  }
	}

	describe ("fromHex") {
		describe ("when given something that's not hex") {
			val string = "Mommy"
			it ("complains") {
				try {
					Utils.fromHex (string)
					fail ()
				}
				catch {
					case e: IllegalArgumentException => assert (e.getMessage === "'Mommy' cannot be converted from hex")
				}
			}
		}
    describe ("when given a hex number starting with 0x") {
      val string = "0x45"
      it ("converts fine") {
        assert (Utils.fromHex (string) === 69)
      }
    }
    describe ("when given a hex number not starting with 0x") {
      val string = "CDeF"
      it ("converts fine") {
        assert (Utils.fromHex (string) === 52719)
      }
    }
	}

	describe ("valueIsTooWide") {
	  describe ("when value is not too wide") {
	    val result = Utils.valueIsTooWide(0x7F, 7)
	    it ("returns false") {
	      assert (result === false)
	    }
	  }
	  describe ("when value is too wide") {
	    val result = Utils.valueIsTooWide (0xFF, 7)
	    it ("returns true") {
	      assert (result === true)
	    }
	  }
	}
	describe ("bitRange") {
	  describe ("for a single-bit field") {
	    val result = Utils.bitRange (3, 1)
	    it ("returns singular") {
	      assert (result === "bit 3")
	    }
	  }
	  describe ("for a multi-bit field") {
	    val result = Utils.bitRange (3, 4)
	    it ("returns plural") {
	      assert (result === "bits 3-6")
	    }
	  }
	}
	describe ("join") {
	  describe ("when given an empty list") {
	    val result = Utils.join (Nil, "blah")
	    it ("returns an empty string") {
	      assert (result === "")
	    }
	  }
	  describe ("when given a single-element list") {
	    val result = Utils.join (List (false), "blah")
	    it ("returns that element") {
	      assert (result === "false")
	    }
	  }
	  describe ("when given a multi-element list") {
	    val result = Utils.join (List (1, 3, 5), " & ")
	    it ("separates elements with delimiter") {
	      assert (result === "1 & 3 & 5")
	    }
	  }
	}
	describe ("makeMask") {
	  describe ("when given a zero bit count") {
	    val result = Utils.makeMask (4, 0)
	    it ("returns all zeros") {
	      assert (result === 0)
	    }
	  }
	  describe ("when given reasonable parameters") {
	    val result = Utils.makeMask (21, 6)
	    it ("returns reasonable results") {
	      assert (result === 0x07E00000)
	    }
	  }
	  describe ("when given a ridiculously large bit count") {
	    val result = Utils.makeMask (4, 10000)
	    it ("returns only 32 bits") {
	      assert (result === 0xFFFFFFF0)
	    }
	  }
	}
}