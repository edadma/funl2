package xyz.hyperreal.funl2

import org.scalatest._
import prop.PropertyChecks


class left_assoc_Tests extends FreeSpec with PropertyChecks with Matchers with HaskellTest {

	"-1" in {
		test( """(a|ab)(c|bcd)(d*)""", """abcd""", "(0,4)(0,1)(1,4)(4,4)" ) shouldBe true
	}

	"-2" in {
		test( """(a|ab)(bcd|c)(d*)""", """abcd""", "(0,4)(0,1)(1,4)(4,4)" ) shouldBe true
	}

	"-3" in {
		test( """(ab|a)(c|bcd)(d*)""", """abcd""", "(0,4)(0,1)(1,4)(4,4)" ) shouldBe true
	}

	"-4" in {
		test( """(ab|a)(bcd|c)(d*)""", """abcd""", "(0,4)(0,1)(1,4)(4,4)" ) shouldBe true
	}

	"-5" in {
		test( """(a*)(b|abc)(c*)""", """abc""", "(0,3)(0,0)(0,3)(3,3)" ) shouldBe true
	}

	"-6" in {
		test( """(a*)(abc|b)(c*)""", """abc""", "(0,3)(0,0)(0,3)(3,3)" ) shouldBe true
	}

	"-7" in {
		test( """(a*)(b|abc)(c*)""", """abc""", "(0,3)(0,0)(0,3)(3,3)" ) shouldBe true
	}

	"-8" in {
		test( """(a*)(abc|b)(c*)""", """abc""", "(0,3)(0,0)(0,3)(3,3)" ) shouldBe true
	}

	"-9" in {
		test( """(a|ab)(c|bcd)(d|.*)""", """abcd""", "(0,4)(0,1)(1,4)(4,4)" ) shouldBe true
	}

	"-10" in {
		test( """(a|ab)(bcd|c)(d|.*)""", """abcd""", "(0,4)(0,1)(1,4)(4,4)" ) shouldBe true
	}

	"-11" in {
		test( """(ab|a)(c|bcd)(d|.*)""", """abcd""", "(0,4)(0,1)(1,4)(4,4)" ) shouldBe true
	}

	"-12" in {
		test( """(ab|a)(bcd|c)(d|.*)""", """abcd""", "(0,4)(0,1)(1,4)(4,4)" ) shouldBe true
	}
}
