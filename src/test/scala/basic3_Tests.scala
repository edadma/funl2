package xyz.hyperreal.funl2

import org.scalatest._
import prop.PropertyChecks


class basic3_Tests extends FreeSpec with PropertyChecks with Matchers with HaskellTest {

	"1" in {
		test( """\)""", """()""", "(1,2)" ) shouldBe true
	}

	"2" in {
		test( """\}""", """}""", "(0,1)" ) shouldBe true
	}

	"3" in {
		test( """]""", """]""", "(0,1)" ) shouldBe true
	}

	"4" in {
		test( """$^""", """NULL""", "(0,0)" ) shouldBe true
	}

	"5" in {
		test( """a($)""", """aa""", "(1,2)(2,2)" ) shouldBe true
	}

	"6" in {
		test( """a*(^a)""", """aa""", "(0,1)(0,1)" ) shouldBe true
	}

	"7" in {
		test( """(..)*(...)*""", """a""", "(0,0)(?,?)(?,?)" ) shouldBe true
	}

	"8" in {
		test( """(..)*(...)*""", """abcd""", "(0,4)(2,4)(?,?)" ) shouldBe true
	}

	"9" in {
		test( """(ab|a)(bc|c)""", """abc""", "(0,3)(0,2)(2,3)" ) shouldBe true
	}

	"10" in {
		test( """(ab)c|abc""", """abc""", "(0,3)(0,2)" ) shouldBe true
	}

	"11" in {
		test( """a{0}b""", """ab""", "(1,2)" ) shouldBe true
	}

	"12" in {
		test( """(a*)(b?)(b+)b{3}""", """aaabbbbbbb""", "(0,10)(0,3)(3,4)(4,7)" ) shouldBe true
	}

	"13" in {
		test( """(a*)(b{0,1})(b{1,})b{3}""", """aaabbbbbbb""", "(0,10)(0,3)(3,4)(4,7)" ) shouldBe true
	}

	"15" in {
		test( """((a|a)|a)""", """a""", "(0,1)(0,1)(0,1)" ) shouldBe true
	}

	"16" in {
		test( """(a*)(a|aa)""", """aaaa""", "(0,4)(0,3)(3,4)" ) shouldBe true
	}

	"17" in {
		test( """a*(a.|aa)""", """aaaa""", "(0,4)(2,4)" ) shouldBe true
	}

	"18" in {
		test( """a(b)|c(d)|a(e)f""", """aef""", "(0,3)(?,?)(?,?)(1,2)" ) shouldBe true
	}

	"19" in {
		test( """(a|b)?.*""", """b""", "(0,1)(0,1)" ) shouldBe true
	}

	"20" in {
		test( """(a|b)c|a(b|c)""", """ac""", "(0,2)(0,1)(?,?)" ) shouldBe true
	}

	"21" in {
		test( """(a|b)c|a(b|c)""", """ab""", "(0,2)(?,?)(1,2)" ) shouldBe true
	}

	"22" in {
		test( """(a|b)*c|(a|ab)*c""", """abc""", "(0,3)(1,2)(?,?)" ) shouldBe true
	}

	"23" in {
		test( """(a|b)*c|(a|ab)*c""", """xc""", "(1,2)(?,?)(?,?)" ) shouldBe true
	}

	"24" in {
		test( """(.a|.b).*|.*(.a|.b)""", """xa""", "(0,2)(0,2)(?,?)" ) shouldBe true
	}

	"25" in {
		test( """a?(ab|ba)ab""", """abab""", "(0,4)(0,2)" ) shouldBe true
	}

	"26" in {
		test( """a?(ac{0}b|ba)ab""", """abab""", "(0,4)(0,2)" ) shouldBe true
	}

	"27" in {
		test( """ab|abab""", """abbabab""", "(0,2)" ) shouldBe true
	}

	"28" in {
		test( """aba|bab|bba""", """baaabbbaba""", "(5,8)" ) shouldBe true
	}

	"29" in {
		test( """aba|bab""", """baaabbbaba""", "(6,9)" ) shouldBe true
	}

	"30" in {
		test( """(aa|aaa)*|(a|aaaaa)""", """aa""", "(0,2)(0,2)(?,?)" ) shouldBe true
	}

	"31" in {
		test( """(a.|.a.)*|(a|.a...)""", """aa""", "(0,2)(0,2)(?,?)" ) shouldBe true
	}

	"32" in {
		test( """ab|a""", """xabc""", "(1,3)" ) shouldBe true
	}

	"33" in {
		test( """ab|a""", """xxabc""", "(2,4)" ) shouldBe true
	}

	"34" in {
		test( """(aB|cD)*""", """aBcD""", "(0,4)(2,4)" ) shouldBe true
	}

	"35" in {
		test( """:::1:::0:|:::1:1:0:""", """:::0:::1:::1:::0:""", "(8,17)" ) shouldBe true
	}

	"36" in {
		test( """:::1:::0:|:::1:1:1:""", """:::0:::1:::1:::0:""", "(8,17)" ) shouldBe true
	}

	"37" in {
		test( """[[:lower:]]+""", """`az{""", "(1,3)" ) shouldBe true
	}

	"38" in {
		test( """[[:upper:]]+""", """@AZ[""", "(1,3)" ) shouldBe true
	}

	"39" in {
		test( """(a)(b)(c)""", """abc""", "(0,3)(0,1)(1,2)(2,3)" ) shouldBe true
	}

	"43" in {
		test( """((((((((((((((((((((((((((((((x))))))))))))))))))))))))))))))""", """x""", "(0,1)(0,1)(0,1)(0,1)(0,1)(0,1)(0,1)(0,1)(0,1)(0,1)(0,1)(0,1)(0,1)(0,1)(0,1)(0,1)(0,1)(0,1)(0,1)(0,1)(0,1)(0,1)(0,1)(0,1)(0,1)(0,1)(0,1)(0,1)(0,1)(0,1)(0,1)" ) shouldBe true
	}

	"44" in {
		test( """((((((((((((((((((((((((((((((x))))))))))))))))))))))))))))))*""", """xx""", "(0,2)(1,2)(1,2)(1,2)(1,2)(1,2)(1,2)(1,2)(1,2)(1,2)(1,2)(1,2)(1,2)(1,2)(1,2)(1,2)(1,2)(1,2)(1,2)(1,2)(1,2)(1,2)(1,2)(1,2)(1,2)(1,2)(1,2)(1,2)(1,2)(1,2)(1,2)" ) shouldBe true
	}

	"45" in {
		test( """a?(ab|ba)*""", """ababababababababababababababababababababababababababababababababababababababababa""", "(0,81)(79,81)" ) shouldBe true
	}

	"46" in {
		test( """abaa|abbaa|abbbaa|abbbbaa""", """ababbabbbabbbabbbbabbbbaa""", "(18,25)" ) shouldBe true
	}

	"47" in {
		test( """abaa|abbaa|abbbaa|abbbbaa""", """ababbabbbabbbabbbbabaa""", "(18,22)" ) shouldBe true
	}

	"48" in {
		test( """aaac|aabc|abac|abbc|baac|babc|bbac|bbbc""", """baaabbbabac""", "(7,11)" ) shouldBe true
	}

	"49" in {
		test( """aaaa|bbbb|cccc|ddddd|eeeeee|fffffff|gggg|hhhh|iiiii|jjjjj|kkkkk|llll""", """XaaaXbbbXcccXdddXeeeXfffXgggXhhhXiiiXjjjXkkkXlllXcbaXaaaa""", "(53,57)" ) shouldBe true
	}

	"50" in {
		test( """a*a*a*a*a*b""", """aaaaaaaaab""", "(0,10)" ) shouldBe true
	}

	"51" in {
		test( """ab+bc""", """abbc""", "(0,4)" ) shouldBe true
	}

	"52" in {
		test( """ab+bc""", """abbbbc""", "(0,6)" ) shouldBe true
	}

	"53" in {
		test( """ab?bc""", """abbc""", "(0,4)" ) shouldBe true
	}

	"54" in {
		test( """ab?bc""", """abc""", "(0,3)" ) shouldBe true
	}

	"55" in {
		test( """ab?c""", """abc""", "(0,3)" ) shouldBe true
	}

	"56" in {
		test( """ab|cd""", """abc""", "(0,2)" ) shouldBe true
	}

	"57" in {
		test( """ab|cd""", """abcd""", "(0,2)" ) shouldBe true
	}

	"58" in {
		test( """a\(b""", """a(b""", "(0,3)" ) shouldBe true
	}

	"59" in {
		test( """a\(*b""", """ab""", "(0,2)" ) shouldBe true
	}

	"60" in {
		test( """a\(*b""", """a((b""", "(0,4)" ) shouldBe true
	}

	"61" in {
		test( """((a))""", """abc""", "(0,1)(0,1)(0,1)" ) shouldBe true
	}

	"62" in {
		test( """(a)b(c)""", """abc""", "(0,3)(0,1)(2,3)" ) shouldBe true
	}

	"63" in {
		test( """a+b+c""", """aabbabc""", "(4,7)" ) shouldBe true
	}

	"64" in {
		test( """a*""", """aaa""", "(0,3)" ) shouldBe true
	}

	"65" in {
		test( """(a*)*""", """-""", "(0,0)(0,0)" ) shouldBe true
	}

	"66" in {
		test( """(a*)+""", """-""", "(0,0)(0,0)" ) shouldBe true
	}

	"67" in {
		test( """(a*|b)*""", """-""", "(0,0)(0,0)" ) shouldBe true
	}

	"68" in {
		test( """(a+|b)*""", """ab""", "(0,2)(1,2)" ) shouldBe true
	}

	"69" in {
		test( """(a+|b)+""", """ab""", "(0,2)(1,2)" ) shouldBe true
	}

	"70" in {
		test( """(a+|b)?""", """ab""", "(0,1)(0,1)" ) shouldBe true
	}

	"71" in {
		test( """(^)*""", """-""", "(0,0)(0,0)" ) shouldBe true
	}

	"72" in {
		test( """([abc])*d""", """abbbcd""", "(0,6)(4,5)" ) shouldBe true
	}

	"73" in {
		test( """([abc])*bcd""", """abcd""", "(0,4)(0,1)" ) shouldBe true
	}

	"74" in {
		test( """a|b|c|d|e""", """e""", "(0,1)" ) shouldBe true
	}

	"75" in {
		test( """(a|b|c|d|e)f""", """ef""", "(0,2)(0,1)" ) shouldBe true
	}

	"76" in {
		test( """((a*|b))*""", """-""", "(0,0)(0,0)(0,0)" ) shouldBe true
	}

	"77" in {
		test( """(ab|cd)e""", """abcde""", "(2,5)(2,4)" ) shouldBe true
	}

	"78" in {
		test( """(a|b)c*d""", """abcd""", "(1,4)(1,2)" ) shouldBe true
	}

	"79" in {
		test( """(ab|ab*)bc""", """abc""", "(0,3)(0,1)" ) shouldBe true
	}

	"80" in {
		test( """a([bc]*)c*""", """abc""", "(0,3)(1,3)" ) shouldBe true
	}

	"81" in {
		test( """a([bc]*)(c*d)""", """abcd""", "(0,4)(1,3)(3,4)" ) shouldBe true
	}

	"82" in {
		test( """a([bc]+)(c*d)""", """abcd""", "(0,4)(1,3)(3,4)" ) shouldBe true
	}

	"83" in {
		test( """a([bc]*)(c+d)""", """abcd""", "(0,4)(1,2)(2,4)" ) shouldBe true
	}

	"84" in {
		test( """a[bcd]*dcdcde""", """adcdcde""", "(0,7)" ) shouldBe true
	}

	"85" in {
		test( """(ab|a)b*c""", """abc""", "(0,3)(0,2)" ) shouldBe true
	}

	"86" in {
		test( """((a)(b)c)(d)""", """abcd""", "(0,4)(0,3)(0,1)(1,2)(3,4)" ) shouldBe true
	}

	"87" in {
		test( """^a(bc+|b[eh])g|.h$""", """abh""", "(1,3)(?,?)" ) shouldBe true
	}

	"88" in {
		test( """(bc+d$|ef*g.|h?i(j|k))""", """effgz""", "(0,5)(0,5)(?,?)" ) shouldBe true
	}

	"89" in {
		test( """(bc+d$|ef*g.|h?i(j|k))""", """ij""", "(0,2)(0,2)(1,2)" ) shouldBe true
	}

	"90" in {
		test( """(bc+d$|ef*g.|h?i(j|k))""", """reffgz""", "(1,6)(1,6)(?,?)" ) shouldBe true
	}

	"91" in {
		test( """(((((((((a)))))))))""", """a""", "(0,1)(0,1)(0,1)(0,1)(0,1)(0,1)(0,1)(0,1)(0,1)(0,1)" ) shouldBe true
	}

	"92" in {
		test( """(.*)c(.*)""", """abcde""", "(0,5)(0,2)(3,5)" ) shouldBe true
	}

	"93" in {
		test( """a(bc)d""", """abcd""", "(0,4)(1,3)" ) shouldBe true
	}

	"94" in {
		test( """a[-]?c""", """ac""", "(0,3)" ) shouldBe true
	}

	"95" in {
		test( """M[ou]'?am+[ae]r_.*([AEae]l[-_])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]""", """Muammar_Qaddafi""", "(0,15)(?,?)(10,12)" ) shouldBe true
	}

	"96" in {
		test( """M[ou]'?am+[ae]r_.*([AEae]l[-_])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]""", """Mo'ammar_Gadhafi""", "(0,16)(?,?)(11,13)" ) shouldBe true
	}

	"97" in {
		test( """M[ou]'?am+[ae]r_.*([AEae]l[-_])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]""", """Muammar_Kaddafi""", "(0,15)(?,?)(10,12)" ) shouldBe true
	}

	"98" in {
		test( """M[ou]'?am+[ae]r_.*([AEae]l[-_])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]""", """Muammar_Qadhafi""", "(0,15)(?,?)(10,12)" ) shouldBe true
	}

	"99" in {
		test( """M[ou]'?am+[ae]r_.*([AEae]l[-_])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]""", """Muammar_Gadafi""", "(0,14)(?,?)(10,11)" ) shouldBe true
	}

	"100" in {
		test( """M[ou]'?am+[ae]r_.*([AEae]l[-_])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]""", """Mu'ammar_Qadafi""", "(0,15)(?,?)(11,12)" ) shouldBe true
	}

	"101" in {
		test( """M[ou]'?am+[ae]r_.*([AEae]l[-_])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]""", """Moamar_Gaddafi""", "(0,14)(?,?)(9,11)" ) shouldBe true
	}

	"102" in {
		test( """M[ou]'?am+[ae]r_.*([AEae]l[-_])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]""", """Mu'ammar_Qadhdhafi""", "(0,18)(?,?)(13,15)" ) shouldBe true
	}

	"103" in {
		test( """M[ou]'?am+[ae]r_.*([AEae]l[-_])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]""", """Muammar_Khaddafi""", "(0,16)(?,?)(11,13)" ) shouldBe true
	}

	"104" in {
		test( """M[ou]'?am+[ae]r_.*([AEae]l[-_])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]""", """Muammar_Ghaddafy""", "(0,16)(?,?)(11,13)" ) shouldBe true
	}

	"105" in {
		test( """M[ou]'?am+[ae]r_.*([AEae]l[-_])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]""", """Muammar_Ghadafi""", "(0,15)(?,?)(11,12)" ) shouldBe true
	}

	"106" in {
		test( """M[ou]'?am+[ae]r_.*([AEae]l[-_])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]""", """Muammar_Ghaddafi""", "(0,16)(?,?)(11,13)" ) shouldBe true
	}

	"107" in {
		test( """M[ou]'?am+[ae]r_.*([AEae]l[-_])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]""", """Muamar_Kaddafi""", "(0,14)(?,?)(9,11)" ) shouldBe true
	}

	"108" in {
		test( """M[ou]'?am+[ae]r_.*([AEae]l[-_])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]""", """Muammar_Quathafi""", "(0,16)(?,?)(11,13)" ) shouldBe true
	}

	"109" in {
		test( """M[ou]'?am+[ae]r_.*([AEae]l[-_])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]""", """Muammar_Gheddafi""", "(0,16)(?,?)(11,13)" ) shouldBe true
	}

	"110" in {
		test( """M[ou]'?am+[ae]r_.*([AEae]l[-_])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]""", """Moammar_Khadafy""", "(0,15)(?,?)(11,12)" ) shouldBe true
	}

	"111" in {
		test( """M[ou]'?am+[ae]r_.*([AEae]l[-_])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]""", """Moammar_Qudhafi""", "(0,15)(?,?)(10,12)" ) shouldBe true
	}

	"112" in {
		test( """a+(b|c)*d+""", """aabcdd""", "(0,6)(3,4)" ) shouldBe true
	}

	"113" in {
		test( """^.+$""", """vivi""", "(0,4)" ) shouldBe true
	}

	"114" in {
		test( """^(.+)$""", """vivi""", "(0,4)(0,4)" ) shouldBe true
	}

	"115" in {
		test( """^([^!.]+).att.com!(.+)$""", """gryphon.att.com!eby""", "(0,19)(0,7)(16,19)" ) shouldBe true
	}

	"116" in {
		test( """^([^!]+!)?([^!]+)$""", """bas""", "(0,3)(?,?)(0,3)" ) shouldBe true
	}

	"117" in {
		test( """^([^!]+!)?([^!]+)$""", """bar!bas""", "(0,7)(0,4)(4,7)" ) shouldBe true
	}

	"118" in {
		test( """^([^!]+!)?([^!]+)$""", """foo!bas""", "(0,7)(0,4)(4,7)" ) shouldBe true
	}

	"119" in {
		test( """^.+!([^!]+!)([^!]+)$""", """foo!bar!bas""", "(0,11)(4,8)(8,11)" ) shouldBe true
	}

	"120" in {
		test( """((foo)|(bar))!bas""", """bar!bas""", "(0,7)(0,3)(?,?)(0,3)" ) shouldBe true
	}

	"121" in {
		test( """((foo)|(bar))!bas""", """foo!bar!bas""", "(4,11)(4,7)(?,?)(4,7)" ) shouldBe true
	}

	"122" in {
		test( """((foo)|(bar))!bas""", """foo!bas""", "(0,7)(0,3)(0,3)(?,?)" ) shouldBe true
	}

	"123" in {
		test( """((foo)|bar)!bas""", """bar!bas""", "(0,7)(0,3)(?,?)" ) shouldBe true
	}

	"124" in {
		test( """((foo)|bar)!bas""", """foo!bar!bas""", "(4,11)(4,7)(?,?)" ) shouldBe true
	}

	"125" in {
		test( """((foo)|bar)!bas""", """foo!bas""", "(0,7)(0,3)(0,3)" ) shouldBe true
	}

	"126" in {
		test( """(foo|(bar))!bas""", """bar!bas""", "(0,7)(0,3)(0,3)" ) shouldBe true
	}

	"127" in {
		test( """(foo|(bar))!bas""", """foo!bar!bas""", "(4,11)(4,7)(4,7)" ) shouldBe true
	}

	"128" in {
		test( """(foo|(bar))!bas""", """foo!bas""", "(0,7)(0,3)(?,?)" ) shouldBe true
	}

	"129" in {
		test( """(foo|bar)!bas""", """bar!bas""", "(0,7)(0,3)" ) shouldBe true
	}

	"130" in {
		test( """(foo|bar)!bas""", """foo!bar!bas""", "(4,11)(4,7)" ) shouldBe true
	}

	"131" in {
		test( """(foo|bar)!bas""", """foo!bas""", "(0,7)(0,3)" ) shouldBe true
	}

	"132" in {
		test( """^(([^!]+!)?([^!]+)|.+!([^!]+!)([^!]+))$""", """foo!bar!bas""", "(0,11)(0,11)(?,?)(?,?)(4,8)(8,11)" ) shouldBe true
	}

	"133" in {
		test( """^([^!]+!)?([^!]+)$|^.+!([^!]+!)([^!]+)$""", """bas""", "(0,3)(?,?)(0,3)(?,?)(?,?)" ) shouldBe true
	}

	"134" in {
		test( """^([^!]+!)?([^!]+)$|^.+!([^!]+!)([^!]+)$""", """bar!bas""", "(0,7)(0,4)(4,7)(?,?)(?,?)" ) shouldBe true
	}

	"135" in {
		test( """^([^!]+!)?([^!]+)$|^.+!([^!]+!)([^!]+)$""", """foo!bar!bas""", "(0,11)(?,?)(?,?)(4,8)(8,11)" ) shouldBe true
	}

	"136" in {
		test( """^([^!]+!)?([^!]+)$|^.+!([^!]+!)([^!]+)$""", """foo!bas""", "(0,7)(0,4)(4,7)(?,?)(?,?)" ) shouldBe true
	}

	"137" in {
		test( """^(([^!]+!)?([^!]+)|.+!([^!]+!)([^!]+))$""", """bas""", "(0,3)(0,3)(?,?)(0,3)(?,?)(?,?)" ) shouldBe true
	}

	"138" in {
		test( """^(([^!]+!)?([^!]+)|.+!([^!]+!)([^!]+))$""", """bar!bas""", "(0,7)(0,7)(0,4)(4,7)(?,?)(?,?)" ) shouldBe true
	}

	"139" in {
		test( """^(([^!]+!)?([^!]+)|.+!([^!]+!)([^!]+))$""", """foo!bar!bas""", "(0,11)(0,11)(?,?)(?,?)(4,8)(8,11)" ) shouldBe true
	}

	"140" in {
		test( """^(([^!]+!)?([^!]+)|.+!([^!]+!)([^!]+))$""", """foo!bas""", "(0,7)(0,7)(0,4)(4,7)(?,?)(?,?)" ) shouldBe true
	}

	"141" in {
		test( """.*(/XXX).*""", """/XXX""", "(0,4)(0,4)" ) shouldBe true
	}

	"142" in {
		test( """.*(\\XXX).*""", """\XXX""", "(0,4)(0,4)" ) shouldBe true
	}

	"143" in {
		test( """\\XXX""", """\XXX""", "(0,4)" ) shouldBe true
	}

	"144" in {
		test( """.*(/000).*""", """/000""", "(0,4)(0,4)" ) shouldBe true
	}

	"145" in {
		test( """.*(\\000).*""", """\000""", "(0,4)(0,4)" ) shouldBe true
	}

	"146" in {
		test( """\\000""", """\000""", "(0,4)" ) shouldBe true
	}
}
