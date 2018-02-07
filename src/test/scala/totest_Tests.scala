package xyz.hyperreal.funl2

import org.scalatest._
import prop.PropertyChecks


class totest_Tests extends FreeSpec with PropertyChecks with Matchers with HaskellTest {

	"01" in {
		test( """a+""", """xaax""", "(1,3)" ) shouldBe true
	}

	"03" in {
		test( """(a?)((ab)?)""", """ab""", "(0,2)(0,0)(0,2)(0,2)" ) shouldBe true
	}

	"04" in {
		test( """(a?)((ab)?)(b?)""", """ab""", "(0,2)(0,1)(1,1)(?,?)(1,2)" ) shouldBe true
	}

	"05" in {
		test( """((a?)((ab)?))(b?)""", """ab""", "(0,2)(0,2)(0,0)(0,2)(0,2)(2,2)" ) shouldBe true
	}

	"06" in {
		test( """(a?)(((ab)?)(b?))""", """ab""", "(0,2)(0,1)(1,2)(1,1)(?,?)(1,2)" ) shouldBe true
	}

	"07" in {
		test( """(.?)""", """x""", "(0,1)(0,1)" ) shouldBe true
	}

	"08" in {
		test( """(.?){1}""", """x""", "(0,1)(0,1)" ) shouldBe true
	}

	"09" in {
		test( """(.?)(.?)""", """x""", "(0,1)(0,1)(1,1)" ) shouldBe true
	}

	"10" in {
		test( """(.?){2}""", """x""", "(0,1)(1,1)" ) shouldBe true
	}

	"11" in {
		test( """(.?)*""", """x""", "(0,1)(0,1)" ) shouldBe true
	}

	"12" in {
		test( """(.?.?)""", """xxx""", "(0,2)(0,2)" ) shouldBe true
	}

	"13" in {
		test( """(.?.?){1}""", """xxx""", "(0,2)(0,2)" ) shouldBe true
	}

	"14" in {
		test( """(.?.?)(.?.?)""", """xxx""", "(0,3)(0,2)(2,3)" ) shouldBe true
	}

	"15" in {
		test( """(.?.?){2}""", """xxx""", "(0,3)(2,3)" ) shouldBe true
	}

	"16" in {
		test( """(.?.?)(.?.?)(.?.?)""", """xxx""", "(0,3)(0,2)(2,3)(3,3)" ) shouldBe true
	}

	"17" in {
		test( """(.?.?){3}""", """xxx""", "(0,3)(3,3)" ) shouldBe true
	}

	"18" in {
		test( """(.?.?)*""", """xxx""", "(0,3)(2,3)" ) shouldBe true
	}

	"19" in {
		test( """a?((ab)?)(b?)""", """ab""", "(0,2)(1,1)(?,?)(1,2)" ) shouldBe true
	}

	"20" in {
		test( """(a?)((ab)?)b?""", """ab""", "(0,2)(0,1)(1,1)(?,?)" ) shouldBe true
	}

	"21" in {
		test( """a?((ab)?)b?""", """ab""", "(0,2)(1,1)(?,?)" ) shouldBe true
	}

	"22" in {
		test( """(a*){2}""", """xxxxx""", "(0,0)(0,0)" ) shouldBe true
	}

	"23" in {
		test( """(ab?)(b?a)""", """aba""", "(0,3)(0,2)(2,3)" ) shouldBe true
	}

	"24" in {
		test( """(a|ab)(ba|a)""", """aba""", "(0,3)(0,2)(2,3)" ) shouldBe true
	}

	"25" in {
		test( """(a|ab|ba)""", """aba""", "(0,2)(0,2)" ) shouldBe true
	}

	"26" in {
		test( """(a|ab|ba)(a|ab|ba)""", """aba""", "(0,3)(0,2)(2,3)" ) shouldBe true
	}

	"27" in {
		test( """(a|ab|ba)*""", """aba""", "(0,3)(2,3)" ) shouldBe true
	}

	"28" in {
		test( """(aba|a*b)""", """ababa""", "(0,3)(0,3)" ) shouldBe true
	}

	"29" in {
		test( """(aba|a*b)(aba|a*b)""", """ababa""", "(0,5)(0,2)(2,5)" ) shouldBe true
	}

	"1029" in {
		test( """(aba|a*b)(aba|a*b)(aba|a*b)""", """ababa""", "NOMATCH" ) shouldBe true
	}

	"30" in {
		test( """(aba|a*b)*""", """ababa""", "(0,5)(2,5)" ) shouldBe true
	}

	"31" in {
		test( """(aba|ab|a)""", """ababa""", "(0,3)(0,3)" ) shouldBe true
	}

	"32" in {
		test( """(aba|ab|a)(aba|ab|a)""", """ababa""", "(0,5)(0,2)(2,5)" ) shouldBe true
	}

	"1032" in {
		test( """(aba|ab|a)(aba|ab|a)(aba|ab|a)""", """ababa""", "(0,5)(0,2)(2,4)(4,5)" ) shouldBe true
	}

	"33" in {
		test( """(aba|ab|a)*""", """ababa""", "(0,5)(2,5)" ) shouldBe true
	}

	"34" in {
		test( """(a(b)?)""", """aba""", "(0,2)(0,2)(1,2)" ) shouldBe true
	}

	"35" in {
		test( """(a(b)?)(a(b)?)""", """aba""", "(0,3)(0,2)(1,2)(2,3)(?,?)" ) shouldBe true
	}

	"36" in {
		test( """(a(b)?)+""", """aba""", "(0,3)(2,3)(?,?)" ) shouldBe true
	}

	"37" in {
		test( """(.*)(.*)""", """xx""", "(0,2)(0,2)(2,2)" ) shouldBe true
	}

	"38" in {
		test( """.*(.*)""", """xx""", "(0,2)(2,2)" ) shouldBe true
	}

	"39" in {
		test( """(a.*z|b.*y)""", """azbazby""", "(0,5)(0,5)" ) shouldBe true
	}

	"40" in {
		test( """(a.*z|b.*y)(a.*z|b.*y)""", """azbazby""", "(0,7)(0,5)(5,7)" ) shouldBe true
	}

	"41" in {
		test( """(a.*z|b.*y)*""", """azbazby""", "(0,7)(5,7)" ) shouldBe true
	}

	"42" in {
		test( """(.|..)(.*)""", """ab""", "(0,2)(0,2)(2,2)" ) shouldBe true
	}

	"43" in {
		test( """((..)*(...)*)""", """xxx""", "(0,3)(0,3)(?,?)(0,3)" ) shouldBe true
	}

	"44" in {
		test( """((..)*(...)*)((..)*(...)*)""", """xxx""", "(0,3)(0,3)(?,?)(0,3)(3,3)(?,?)(?,?)" ) shouldBe true
	}

	"45" in {
		test( """((..)*(...)*)*""", """xxx""", "(0,3)(0,3)(?,?)(0,3)" ) shouldBe true
	}

	"83" in {
		test( """(aa(b(b))?)+""", """aabbaa""", "(0,6)(4,6)(?,?)(?,?)" ) shouldBe true
	}

	"84" in {
		test( """(a(b)?)+""", """aba""", "(0,3)(2,3)(?,?)" ) shouldBe true
	}

	"85" in {
		test( """([ab]+)([bc]+)([cd]*)""", """abcd""", "(0,4)(0,2)(2,3)(3,4)" ) shouldBe true
	}

	"90" in {
		test( """^(A([^B]*))?(B(.*))?""", """Aa""", "(0,2)(0,2)(1,2)(?,?)(?,?)" ) shouldBe true
	}

	"91" in {
		test( """^(A([^B]*))?(B(.*))?""", """Bb""", "(0,2)(?,?)(?,?)(0,2)(1,2)" ) shouldBe true
	}

	"110" in {
		test( """(^){0,3}""", """a""", "(0,0)(0,0)" ) shouldBe true
	}

	"111" in {
		test( """($){0,3}""", """a""", "(0,0)(?,?)" ) shouldBe true
	}

	"112" in {
		test( """(^){1,3}""", """a""", "(0,0)(0,0)" ) shouldBe true
	}

	"113" in {
		test( """($){1,3}""", """a""", "(1,1)(1,1)" ) shouldBe true
	}

	"200" in {
		test( """((s^)|(s)|(^)|($)|(^.))*""", """searchme""", "(0,1)(0,1)(?,?)(0,1)(?,?)(?,?)(?,?)" ) shouldBe true
	}

	"201" in {
		test( """s(()|^)e""", """searchme""", "(0,2)(1,1)(1,1)" ) shouldBe true
	}

	"202" in {
		test( """s(^|())e""", """searchme""", "(0,2)(1,1)(1,1)" ) shouldBe true
	}

	"203" in {
		test( """s(^|())e""", """searchme""", "(0,2)(1,1)(1,1)" ) shouldBe true
	}

	"204" in {
		test( """s()?e""", """searchme""", "(0,2)(1,1)" ) shouldBe true
	}

	"205" in {
		test( """s(^)?e""", """searchme""", "(0,2)(?,?)" ) shouldBe true
	}

	"206" in {
		test( """((s)|(e)|(a))*""", """searchme""", "(0,3)(2,3)(?,?)(?,?)(2,3)" ) shouldBe true
	}

	"207" in {
		test( """((s)|(e)|())*""", """searchme""", "(0,2)(1,2)(?,?)(1,2)(?,?)" ) shouldBe true
	}

	"208" in {
		test( """((b*)|c(c*))*""", """cbb""", "(0,3)(1,3)(1,3)(?,?)" ) shouldBe true
	}

	"209" in {
		test( """(yyy|(x?)){2,4}""", """yyyyyy""", "(0,6)(3,6)(?,?)" ) shouldBe true
	}

	"210" in {
		test( """($)|()""", """xxx""", "(0,0)(?,?)(0,0)" ) shouldBe true
	}

	"211" in {
		test( """$()|^()""", """ac\n""", "(0,0)(?,?)(0,0)" ) shouldBe true
	}

	"212" in {
		test( """^()|$()""", """ac\n""", "(0,0)(0,0)(?,?)" ) shouldBe true
	}

	"213" in {
		test( """($)?(.)""", """__""", "(0,1)(?,?)(0,1)" ) shouldBe true
	}

	"214" in {
		test( """(.|()|())*""", """c""", "(0,1)(0,1)(?,?)(?,?)" ) shouldBe true
	}

	"215" in {
		test( """((a)|(b)){2,}""", """ab""", "(0,2)(1,2)(?,?)(1,2)" ) shouldBe true
	}

	"216" in {
		test( """.()|((.)?)""", """NULL""", "(0,0)(?,?)(0,0)(?,?)" ) shouldBe true
	}

	"217" in {
		test( """(.|$){2,}""", """xx""", "(0,2)(1,2)" ) shouldBe true
	}

	"218" in {
		test( """(.|$){2,2}""", """xx""", "(0,2)(1,2)" ) shouldBe true
	}

	"219" in {
		test( """(.){2,}""", """xx""", "(0,2)(1,2)" ) shouldBe true
	}

	"220" in {
		test( """(a|())(b|())(c|())""", """abc""", "(0,3)(0,1)(?,?)(1,2)(?,?)(2,3)(?,?)" ) shouldBe true
	}

	"221" in {
		test( """ab()c|ab()c()""", """abc""", "(0,3)(2,2)(-1,-1)(-1,-1)" ) shouldBe true
	}

	"250" in {
		test( """(b(c)|d(e))*""", """bcde""", "(0,4)(2,4)(-1,-1)(3,4)" ) shouldBe true
	}

	"251" in {
		test( """(a(b)*)*""", """aba""", "(0,3)(2,3)(-1,-1)" ) shouldBe true
	}
}
