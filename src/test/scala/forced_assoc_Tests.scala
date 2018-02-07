package xyz.hyperreal.funl2

import org.scalatest._
import prop.PropertyChecks


class forced_assoc_Tests extends FreeSpec with PropertyChecks with Matchers with HaskellTest {

	"1" in {
		test( """(a|ab)(c|bcd)""", """abcd""", "(0,4)(0,1)(1,4)" ) shouldBe true
	}

	"2" in {
		test( """(a|ab)(bcd|c)""", """abcd""", "(0,4)(0,1)(1,4)" ) shouldBe true
	}

	"3" in {
		test( """(ab|a)(c|bcd)""", """abcd""", "(0,4)(0,1)(1,4)" ) shouldBe true
	}

	"4" in {
		test( """(ab|a)(bcd|c)""", """abcd""", "(0,4)(0,1)(1,4)" ) shouldBe true
	}

	"5" in {
		test( """((a|ab)(c|bcd))(d*)""", """abcd""", "(0,4)(0,4)(0,1)(1,4)(4,4)" ) shouldBe true
	}

	"6" in {
		test( """((a|ab)(bcd|c))(d*)""", """abcd""", "(0,4)(0,4)(0,1)(1,4)(4,4)" ) shouldBe true
	}

	"7" in {
		test( """((ab|a)(c|bcd))(d*)""", """abcd""", "(0,4)(0,4)(0,1)(1,4)(4,4)" ) shouldBe true
	}

	"8" in {
		test( """((ab|a)(bcd|c))(d*)""", """abcd""", "(0,4)(0,4)(0,1)(1,4)(4,4)" ) shouldBe true
	}

	"9" in {
		test( """(a|ab)((c|bcd)(d*))""", """abcd""", "(0,4)(0,2)(2,4)(2,3)(3,4)" ) shouldBe true
	}

	"10" in {
		test( """(a|ab)((bcd|c)(d*))""", """abcd""", "(0,4)(0,2)(2,4)(2,3)(3,4)" ) shouldBe true
	}

	"11" in {
		test( """(ab|a)((c|bcd)(d*))""", """abcd""", "(0,4)(0,2)(2,4)(2,3)(3,4)" ) shouldBe true
	}

	"12" in {
		test( """(ab|a)((bcd|c)(d*))""", """abcd""", "(0,4)(0,2)(2,4)(2,3)(3,4)" ) shouldBe true
	}

	"13" in {
		test( """(a*)(b|abc)""", """abc""", "(0,3)(0,0)(0,3)" ) shouldBe true
	}

	"14" in {
		test( """(a*)(abc|b)""", """abc""", "(0,3)(0,0)(0,3)" ) shouldBe true
	}

	"15" in {
		test( """((a*)(b|abc))(c*)""", """abc""", "(0,3)(0,3)(0,0)(0,3)(3,3)" ) shouldBe true
	}

	"16" in {
		test( """((a*)(abc|b))(c*)""", """abc""", "(0,3)(0,3)(0,0)(0,3)(3,3)" ) shouldBe true
	}

	"17" in {
		test( """(a*)((b|abc)(c*))""", """abc""", "(0,3)(0,1)(1,3)(1,2)(2,3)" ) shouldBe true
	}

	"18" in {
		test( """(a*)((abc|b)(c*))""", """abc""", "(0,3)(0,1)(1,3)(1,2)(2,3)" ) shouldBe true
	}

	"19" in {
		test( """(a*)(b|abc)""", """abc""", "(0,3)(0,0)(0,3)" ) shouldBe true
	}

	"20" in {
		test( """(a*)(abc|b)""", """abc""", "(0,3)(0,0)(0,3)" ) shouldBe true
	}

	"21" in {
		test( """((a*)(b|abc))(c*)""", """abc""", "(0,3)(0,3)(0,0)(0,3)(3,3)" ) shouldBe true
	}

	"22" in {
		test( """((a*)(abc|b))(c*)""", """abc""", "(0,3)(0,3)(0,0)(0,3)(3,3)" ) shouldBe true
	}

	"23" in {
		test( """(a*)((b|abc)(c*))""", """abc""", "(0,3)(0,1)(1,3)(1,2)(2,3)" ) shouldBe true
	}

	"24" in {
		test( """(a*)((abc|b)(c*))""", """abc""", "(0,3)(0,1)(1,3)(1,2)(2,3)" ) shouldBe true
	}

	"25" in {
		test( """(a|ab)""", """ab""", "(0,2)(0,2)" ) shouldBe true
	}

	"26" in {
		test( """(ab|a)""", """ab""", "(0,2)(0,2)" ) shouldBe true
	}

	"27" in {
		test( """(a|ab)(b*)""", """ab""", "(0,2)(0,2)(2,2)" ) shouldBe true
	}

	"28" in {
		test( """(ab|a)(b*)""", """ab""", "(0,2)(0,2)(2,2)" ) shouldBe true
	}
}
