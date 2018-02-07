package xyz.hyperreal.funl2

import org.scalatest._
import prop.PropertyChecks


class nullsub3_Tests extends FreeSpec with PropertyChecks with Matchers with HaskellTest {

	"1" in {
		test( """(a*)*""", """a""", "(0,1)(0,1)" ) shouldBe true
	}

	"2" in {
		test( """(a*)*""", """x""", "(0,0)(0,0)" ) shouldBe true
	}

	"3" in {
		test( """(a*)*""", """aaaaaa""", "(0,6)(0,6)" ) shouldBe true
	}

	"4" in {
		test( """(a*)*""", """aaaaaax""", "(0,6)(0,6)" ) shouldBe true
	}

	"5" in {
		test( """(a*)+""", """a""", "(0,1)(0,1)" ) shouldBe true
	}

	"6" in {
		test( """(a*)+""", """x""", "(0,0)(0,0)" ) shouldBe true
	}

	"7" in {
		test( """(a*)+""", """aaaaaa""", "(0,6)(0,6)" ) shouldBe true
	}

	"8" in {
		test( """(a*)+""", """aaaaaax""", "(0,6)(0,6)" ) shouldBe true
	}

	"9" in {
		test( """(a+)*""", """a""", "(0,1)(0,1)" ) shouldBe true
	}

	"10" in {
		test( """(a+)*""", """x""", "(0,0)(?,?)" ) shouldBe true
	}

	"11" in {
		test( """(a+)*""", """aaaaaa""", "(0,6)(0,6)" ) shouldBe true
	}

	"12" in {
		test( """(a+)*""", """aaaaaax""", "(0,6)(0,6)" ) shouldBe true
	}

	"13" in {
		test( """(a+)+""", """a""", "(0,1)(0,1)" ) shouldBe true
	}

	"14" in {
		test( """(a+)+""", """x""", "NOMATCH" ) shouldBe true
	}

	"15" in {
		test( """(a+)+""", """aaaaaa""", "(0,6)(0,6)" ) shouldBe true
	}

	"16" in {
		test( """(a+)+""", """aaaaaax""", "(0,6)(0,6)" ) shouldBe true
	}

	"17" in {
		test( """([a]*)*""", """a""", "(0,1)(0,1)" ) shouldBe true
	}

	"18" in {
		test( """([a]*)*""", """x""", "(0,0)(0,0)" ) shouldBe true
	}

	"19" in {
		test( """([a]*)*""", """aaaaaa""", "(0,6)(0,6)" ) shouldBe true
	}

	"20" in {
		test( """([a]*)*""", """aaaaaax""", "(0,6)(0,6)" ) shouldBe true
	}

	"21" in {
		test( """([a]*)+""", """a""", "(0,1)(0,1)" ) shouldBe true
	}

	"22" in {
		test( """([a]*)+""", """x""", "(0,0)(0,0)" ) shouldBe true
	}

	"23" in {
		test( """([a]*)+""", """aaaaaa""", "(0,6)(0,6)" ) shouldBe true
	}

	"24" in {
		test( """([a]*)+""", """aaaaaax""", "(0,6)(0,6)" ) shouldBe true
	}

	"25" in {
		test( """([^b]*)*""", """a""", "(0,1)(0,1)" ) shouldBe true
	}

	"26" in {
		test( """([^b]*)*""", """b""", "(0,0)(0,0)" ) shouldBe true
	}

	"27" in {
		test( """([^b]*)*""", """aaaaaa""", "(0,6)(0,6)" ) shouldBe true
	}

	"28" in {
		test( """([^b]*)*""", """aaaaaab""", "(0,6)(0,6)" ) shouldBe true
	}

	"29" in {
		test( """([ab]*)*""", """a""", "(0,1)(0,1)" ) shouldBe true
	}

	"30" in {
		test( """([ab]*)*""", """aaaaaa""", "(0,6)(0,6)" ) shouldBe true
	}

	"31" in {
		test( """([ab]*)*""", """ababab""", "(0,6)(0,6)" ) shouldBe true
	}

	"32" in {
		test( """([ab]*)*""", """bababa""", "(0,6)(0,6)" ) shouldBe true
	}

	"33" in {
		test( """([ab]*)*""", """b""", "(0,1)(0,1)" ) shouldBe true
	}

	"34" in {
		test( """([ab]*)*""", """bbbbbb""", "(0,6)(0,6)" ) shouldBe true
	}

	"35" in {
		test( """([ab]*)*""", """aaaabcde""", "(0,5)(0,5)" ) shouldBe true
	}

	"36" in {
		test( """([^a]*)*""", """b""", "(0,1)(0,1)" ) shouldBe true
	}

	"37" in {
		test( """([^a]*)*""", """bbbbbb""", "(0,6)(0,6)" ) shouldBe true
	}

	"38" in {
		test( """([^a]*)*""", """aaaaaa""", "(0,0)(0,0)" ) shouldBe true
	}

	"39" in {
		test( """([^ab]*)*""", """ccccxx""", "(0,6)(0,6)" ) shouldBe true
	}

	"40" in {
		test( """([^ab]*)*""", """ababab""", "(0,0)(0,0)" ) shouldBe true
	}

	"41" in {
		test( """((z)+|a)*""", """zabcde""", "(0,2)(1,2)(?,?)" ) shouldBe true
	}

	"42" in {
		test( """(a)""", """aaa""", "(0,1)(0,1)" ) shouldBe true
	}

	"46" in {
		test( """(a*)*(x)""", """x""", "(0,1)(0,0)(0,1)" ) shouldBe true
	}

	"47" in {
		test( """(a*)*(x)""", """ax""", "(0,2)(0,1)(1,2)" ) shouldBe true
	}

	"48" in {
		test( """(a*)*(x)""", """axa""", "(0,2)(0,1)(1,2)" ) shouldBe true
	}

	"49" in {
		test( """(a*)+(x)""", """x""", "(0,1)(0,0)(0,1)" ) shouldBe true
	}

	"50" in {
		test( """(a*)+(x)""", """ax""", "(0,2)(0,1)(1,2)" ) shouldBe true
	}

	"51" in {
		test( """(a*)+(x)""", """axa""", "(0,2)(0,1)(1,2)" ) shouldBe true
	}

	"52" in {
		test( """(a*){2}(x)""", """x""", "(0,1)(0,0)(0,1)" ) shouldBe true
	}

	"53" in {
		test( """(a*){2}(x)""", """ax""", "(0,2)(1,1)(1,2)" ) shouldBe true
	}

	"54" in {
		test( """(a*){2}(x)""", """axa""", "(0,2)(1,1)(1,2)" ) shouldBe true
	}
}
