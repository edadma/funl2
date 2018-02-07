package xyz.hyperreal.funl2

import org.scalatest._
import prop.PropertyChecks


class repetition2_Tests extends FreeSpec with PropertyChecks with Matchers with HaskellTest {

	"1" in {
		test( """((..)|(.))""", """NULL""", "NOMATCH" ) shouldBe true
	}

	"2" in {
		test( """((..)|(.))((..)|(.))""", """NULL""", "NOMATCH" ) shouldBe true
	}

	"3" in {
		test( """((..)|(.))((..)|(.))((..)|(.))""", """NULL""", "NOMATCH" ) shouldBe true
	}

	"4" in {
		test( """((..)|(.)){1}""", """NULL""", "NOMATCH" ) shouldBe true
	}

	"5" in {
		test( """((..)|(.)){2}""", """NULL""", "NOMATCH" ) shouldBe true
	}

	"6" in {
		test( """((..)|(.)){3}""", """NULL""", "NOMATCH" ) shouldBe true
	}

	"7" in {
		test( """((..)|(.))*""", """NULL""", "(0,0)(?,?)(?,?)(?,?)" ) shouldBe true
	}

	"8" in {
		test( """((..)|(.))""", """a""", "(0,1)(0,1)(?,?)(0,1)" ) shouldBe true
	}

	"9" in {
		test( """((..)|(.))((..)|(.))""", """a""", "NOMATCH" ) shouldBe true
	}

	"10" in {
		test( """((..)|(.))((..)|(.))((..)|(.))""", """a""", "NOMATCH" ) shouldBe true
	}

	"11" in {
		test( """((..)|(.)){1}""", """a""", "(0,1)(0,1)(?,?)(0,1)" ) shouldBe true
	}

	"12" in {
		test( """((..)|(.)){2}""", """a""", "NOMATCH" ) shouldBe true
	}

	"13" in {
		test( """((..)|(.)){3}""", """a""", "NOMATCH" ) shouldBe true
	}

	"14" in {
		test( """((..)|(.))*""", """a""", "(0,1)(0,1)(?,?)(0,1)" ) shouldBe true
	}

	"15" in {
		test( """((..)|(.))""", """aa""", "(0,2)(0,2)(0,2)(?,?)" ) shouldBe true
	}

	"16" in {
		test( """((..)|(.))((..)|(.))""", """aa""", "(0,2)(0,1)(?,?)(0,1)(1,2)(?,?)(1,2)" ) shouldBe true
	}

	"17" in {
		test( """((..)|(.))((..)|(.))((..)|(.))""", """aa""", "NOMATCH" ) shouldBe true
	}

	"18" in {
		test( """((..)|(.)){1}""", """aa""", "(0,2)(0,2)(0,2)(?,?)" ) shouldBe true
	}

	"19" in {
		test( """((..)|(.)){2}""", """aa""", "(0,2)(1,2)(?,?)(1,2)" ) shouldBe true
	}

	"20" in {
		test( """((..)|(.)){3}""", """aa""", "NOMATCH" ) shouldBe true
	}

	"21" in {
		test( """((..)|(.))*""", """aa""", "(0,2)(0,2)(0,2)(?,?)" ) shouldBe true
	}

	"22" in {
		test( """((..)|(.))""", """aaa""", "(0,2)(0,2)(0,2)(?,?)" ) shouldBe true
	}

	"23" in {
		test( """((..)|(.))((..)|(.))""", """aaa""", "(0,3)(0,2)(0,2)(?,?)(2,3)(?,?)(2,3)" ) shouldBe true
	}

	"24" in {
		test( """((..)|(.))((..)|(.))((..)|(.))""", """aaa""", "(0,3)(0,1)(?,?)(0,1)(1,2)(?,?)(1,2)(2,3)(?,?)(2,3)" ) shouldBe true
	}

	"25" in {
		test( """((..)|(.)){1}""", """aaa""", "(0,2)(0,2)(0,2)(?,?)" ) shouldBe true
	}

	"26" in {
		test( """((..)|(.)){2}""", """aaa""", "(0,3)(2,3)(?,?)(2,3)" ) shouldBe true
	}

	"27" in {
		test( """((..)|(.)){3}""", """aaa""", "(0,3)(2,3)(?,?)(2,3)" ) shouldBe true
	}

	"28" in {
		test( """((..)|(.))*""", """aaa""", "(0,3)(2,3)(?,?)(2,3)" ) shouldBe true
	}

	"29" in {
		test( """((..)|(.))""", """aaaa""", "(0,2)(0,2)(0,2)(?,?)" ) shouldBe true
	}

	"30" in {
		test( """((..)|(.))((..)|(.))""", """aaaa""", "(0,4)(0,2)(0,2)(?,?)(2,4)(2,4)(?,?)" ) shouldBe true
	}

	"31" in {
		test( """((..)|(.))((..)|(.))((..)|(.))""", """aaaa""", "(0,4)(0,2)(0,2)(?,?)(2,3)(?,?)(2,3)(3,4)(?,?)(3,4)" ) shouldBe true
	}

	"32" in {
		test( """((..)|(.)){1}""", """aaaa""", "(0,2)(0,2)(0,2)(?,?)" ) shouldBe true
	}

	"33" in {
		test( """((..)|(.)){2}""", """aaaa""", "(0,4)(2,4)(2,4)(?,?)" ) shouldBe true
	}

	"34" in {
		test( """((..)|(.)){3}""", """aaaa""", "(0,4)(3,4)(?,?)(3,4)" ) shouldBe true
	}

	"35" in {
		test( """((..)|(.))*""", """aaaa""", "(0,4)(2,4)(2,4)(?,?)" ) shouldBe true
	}

	"36" in {
		test( """((..)|(.))""", """aaaaa""", "(0,2)(0,2)(0,2)(?,?)" ) shouldBe true
	}

	"37" in {
		test( """((..)|(.))((..)|(.))""", """aaaaa""", "(0,4)(0,2)(0,2)(?,?)(2,4)(2,4)(?,?)" ) shouldBe true
	}

	"38" in {
		test( """((..)|(.))((..)|(.))((..)|(.))""", """aaaaa""", "(0,5)(0,2)(0,2)(?,?)(2,4)(2,4)(?,?)(4,5)(?,?)(4,5)" ) shouldBe true
	}

	"39" in {
		test( """((..)|(.)){1}""", """aaaaa""", "(0,2)(0,2)(0,2)(?,?)" ) shouldBe true
	}

	"40" in {
		test( """((..)|(.)){2}""", """aaaaa""", "(0,4)(2,4)(2,4)(?,?)" ) shouldBe true
	}

	"41" in {
		test( """((..)|(.)){3}""", """aaaaa""", "(0,5)(4,5)(?,?)(4,5)" ) shouldBe true
	}

	"42" in {
		test( """((..)|(.))*""", """aaaaa""", "(0,5)(4,5)(?,?)(4,5)" ) shouldBe true
	}

	"43" in {
		test( """((..)|(.))""", """aaaaaa""", "(0,2)(0,2)(0,2)(?,?)" ) shouldBe true
	}

	"44" in {
		test( """((..)|(.))((..)|(.))""", """aaaaaa""", "(0,4)(0,2)(0,2)(?,?)(2,4)(2,4)(?,?)" ) shouldBe true
	}

	"45" in {
		test( """((..)|(.))((..)|(.))((..)|(.))""", """aaaaaa""", "(0,6)(0,2)(0,2)(?,?)(2,4)(2,4)(?,?)(4,6)(4,6)(?,?)" ) shouldBe true
	}

	"46" in {
		test( """((..)|(.)){1}""", """aaaaaa""", "(0,2)(0,2)(0,2)(?,?)" ) shouldBe true
	}

	"47" in {
		test( """((..)|(.)){2}""", """aaaaaa""", "(0,4)(2,4)(2,4)(?,?)" ) shouldBe true
	}

	"48" in {
		test( """((..)|(.)){3}""", """aaaaaa""", "(0,6)(4,6)(4,6)(?,?)" ) shouldBe true
	}

	"49" in {
		test( """((..)|(.))*""", """aaaaaa""", "(0,6)(4,6)(4,6)(?,?)" ) shouldBe true
	}

	"100" in {
		test( """X(.?){0,}Y""", """X1234567Y""", "(0,9)(7,8)" ) shouldBe true
	}

	"101" in {
		test( """X(.?){1,}Y""", """X1234567Y""", "(0,9)(7,8)" ) shouldBe true
	}

	"102" in {
		test( """X(.?){2,}Y""", """X1234567Y""", "(0,9)(7,8)" ) shouldBe true
	}

	"103" in {
		test( """X(.?){3,}Y""", """X1234567Y""", "(0,9)(7,8)" ) shouldBe true
	}

	"104" in {
		test( """X(.?){4,}Y""", """X1234567Y""", "(0,9)(7,8)" ) shouldBe true
	}

	"105" in {
		test( """X(.?){5,}Y""", """X1234567Y""", "(0,9)(7,8)" ) shouldBe true
	}

	"106" in {
		test( """X(.?){6,}Y""", """X1234567Y""", "(0,9)(7,8)" ) shouldBe true
	}

	"107" in {
		test( """X(.?){7,}Y""", """X1234567Y""", "(0,9)(7,8)" ) shouldBe true
	}

	"108" in {
		test( """X(.?){8,}Y""", """X1234567Y""", "(0,9)(8,8)" ) shouldBe true
	}

	"110" in {
		test( """X(.?){0,8}Y""", """X1234567Y""", "(0,9)(7,8)" ) shouldBe true
	}

	"111" in {
		test( """X(.?){1,8}Y""", """X1234567Y""", "(0,9)(7,8)" ) shouldBe true
	}

	"112" in {
		test( """X(.?){2,8}Y""", """X1234567Y""", "(0,9)(7,8)" ) shouldBe true
	}

	"113" in {
		test( """X(.?){3,8}Y""", """X1234567Y""", "(0,9)(7,8)" ) shouldBe true
	}

	"114" in {
		test( """X(.?){4,8}Y""", """X1234567Y""", "(0,9)(7,8)" ) shouldBe true
	}

	"115" in {
		test( """X(.?){5,8}Y""", """X1234567Y""", "(0,9)(7,8)" ) shouldBe true
	}

	"116" in {
		test( """X(.?){6,8}Y""", """X1234567Y""", "(0,9)(7,8)" ) shouldBe true
	}

	"117" in {
		test( """X(.?){7,8}Y""", """X1234567Y""", "(0,9)(7,8)" ) shouldBe true
	}

	"118" in {
		test( """X(.?){8,8}Y""", """X1234567Y""", "(0,9)(8,8)" ) shouldBe true
	}

	"260" in {
		test( """(a|ab|c|bcd){0,}(d*)""", """ababcd""", "(0,6)(3,6)(6,6)" ) shouldBe true
	}

	"261" in {
		test( """(a|ab|c|bcd){1,}(d*)""", """ababcd""", "(0,6)(3,6)(6,6)" ) shouldBe true
	}

	"262" in {
		test( """(a|ab|c|bcd){2,}(d*)""", """ababcd""", "(0,6)(3,6)(6,6)" ) shouldBe true
	}

	"263" in {
		test( """(a|ab|c|bcd){3,}(d*)""", """ababcd""", "(0,6)(3,6)(6,6)" ) shouldBe true
	}

	"264" in {
		test( """(a|ab|c|bcd){4,}(d*)""", """ababcd""", "NOMATCH" ) shouldBe true
	}

	"265" in {
		test( """(a|ab|c|bcd){0,10}(d*)""", """ababcd""", "(0,6)(3,6)(6,6)" ) shouldBe true
	}

	"266" in {
		test( """(a|ab|c|bcd){1,10}(d*)""", """ababcd""", "(0,6)(3,6)(6,6)" ) shouldBe true
	}

	"267" in {
		test( """(a|ab|c|bcd){2,10}(d*)""", """ababcd""", "(0,6)(3,6)(6,6)" ) shouldBe true
	}

	"268" in {
		test( """(a|ab|c|bcd){3,10}(d*)""", """ababcd""", "(0,6)(3,6)(6,6)" ) shouldBe true
	}

	"269" in {
		test( """(a|ab|c|bcd){4,10}(d*)""", """ababcd""", "NOMATCH" ) shouldBe true
	}

	"270" in {
		test( """(a|ab|c|bcd)*(d*)""", """ababcd""", "(0,6)(3,6)(6,6)" ) shouldBe true
	}

	"271" in {
		test( """(a|ab|c|bcd)+(d*)""", """ababcd""", "(0,6)(3,6)(6,6)" ) shouldBe true
	}
}
