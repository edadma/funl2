package xyz.hyperreal.funl2

import xyz.hyperreal.bvm._


trait HaskellTest {
	def test( regex: String, str: String, expected: String ): Boolean = {
		val res = Pattern.compile( regex ).allMatches( if (str == "NULL") "" else str )

		if (expected == "NOMATCH")
			res.isEmpty
		else {
			for (m <- res)
				if ((m.values map {case (begin, end, _) => s"($begin,$end)"} mkString) matches expected.replace( '?', '.' ).replace( "(-1,-1)", "(0,0)" ).replace( "(", "\\(" ).replace( ")", "\\)" ))
					return true

			false
		}
	}
}