package xyz.hyperreal.funl2

import org.scalatest._
import prop.PropertyChecks


class FunLPredefTests extends FreeSpec with PropertyChecks with Matchers {

	"swap" in {
		runCapture(
			"""
				|var a = 3
				|var b = 4
				|
				|swap( a, b )
				|write( a, b )
			""".stripMargin
		) shouldBe "4, 3"
	}

	"min/max" in {
		runCapture(
			"""
				|write( min(3, 2, 22/3, 1/2, 1) )
				|write( max(3, 2, 22/3, 1/2, 1) )
					""".stripMargin
		) shouldBe
			"""
				|1/2
				|22/3
			""".stripMargin.trim
	}

}