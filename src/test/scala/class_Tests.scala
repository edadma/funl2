package xyz.hyperreal.funl2

import org.scalatest._
import prop.PropertyChecks


class class_Tests extends FreeSpec with PropertyChecks with Matchers with HaskellTest {

	"1" in {
		test( """aa*""", """xaxaax""", "(1,2)" ) shouldBe true
	}

	"2" in {
		test( """(a*)(ab)*(b*)""", """abc""", "(0,2)(0,1)(?,?)(1,2)" ) shouldBe true
	}

	"-2" in {
		test( """(a*)(ab)*(b*)""", """abc""", "(0,2)(0,0)(0,2)(2,2)" ) shouldBe true
	}

	"3" in {
		test( """((a*)(ab)*)((b*)(a*))""", """aba""", "(0,3)(0,2)(0,0)(0,2)(2,3)(2,2)(2,3)" ) shouldBe true
	}

	"4" in {
		test( """(...?.?)*""", """xxxxxx""", "(0,6)(4,6)" ) shouldBe true
	}

	"5" in {
		test( """(a|ab)(bc|c)""", """abcabc""", "(0,3)(0,2)(2,3)" ) shouldBe true
	}

	"6" in {
		test( """(aba|a*b)(aba|a*b)""", """ababa""", "(0,5)(0,2)(2,5)" ) shouldBe true
	}

	"7" in {
		test( """(a*){2}""", """xxxxx""", "(0,0)(0,0)" ) shouldBe true
	}

	"8" in {
		test( """(a*)*""", """a""", "(0,1)(0,1)" ) shouldBe true
	}

	"9" in {
		test( """(aba|a*b)*""", """ababa""", "(0,5)(2,5)" ) shouldBe true
	}

	"10" in {
		test( """(a(b)?)+""", """aba""", "(0,3)(2,3)(?,?)" ) shouldBe true
	}

	"11" in {
		test( """.*(.*)""", """ab""", "(0,2)(2,2)" ) shouldBe true
	}

	"12" in {
		test( """(a?)((ab)?)(b?)a?(ab)?b?""", """abab""", "(0,4)(0,1)(1,1)(?,?)(1,2)(?,?)" ) shouldBe true
	}

	"-12" in {
		test( """(a?)((ab)?)(b?)a?(ab)?b?""", """abab""", "(0,4)(0,1)(1,1)(?,?)(1,2)(2,4)" ) shouldBe true
	}
}
