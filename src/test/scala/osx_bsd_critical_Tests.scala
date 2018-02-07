package xyz.hyperreal.funl2

import org.scalatest._
import prop.PropertyChecks


class osx_bsd_critical_Tests extends FreeSpec with PropertyChecks with Matchers with HaskellTest {

	"1" in {
		test( """(()|.)(b)""", """ab""", "(0,2)(0,1)(?,?)(1,2)" ) shouldBe true
	}

	"-1" in {
		test( """(()|.)(b)""", """ab""", "(1,2)(1,1)(1,1)(1,2)" ) shouldBe true
	}

	"2" in {
		test( """(()|[ab])(b)""", """ab""", "(0,2)(0,1)(?,?)(1,2)" ) shouldBe true
	}

	"-2" in {
		test( """(()|[ab])(b)""", """ab""", "(1,2)(1,1)(1,1)(1,2)" ) shouldBe true
	}

	"3" in {
		test( """(()|[ab])+b""", """aaab""", "(0,4)(2,3)(?,?)" ) shouldBe true
	}

	"-3" in {
		test( """(()|[ab])+b""", """aaab""", "(3,4)(3,3)(3,3)" ) shouldBe true
	}

	"11" in {
		test( """(.|())(b)""", """ab""", "(0,2)(0,1)(?,?)(1,2)" ) shouldBe true
	}

	"12" in {
		test( """([ab]|())(b)""", """ab""", "(0,2)(0,1)(?,?)(1,2)" ) shouldBe true
	}

	"14" in {
		test( """([ab]|())+b""", """aaab""", "(0,4)(2,3)(?,?)" ) shouldBe true
	}

	"-14" in {
		test( """([ab]|())+b""", """aaab""", "(0,4)(3,3)(3,3)" ) shouldBe true
	}

	"20" in {
		test( """(.?)(b)""", """ab""", "(0,2)(0,1)(1,2)" ) shouldBe true
	}
}
