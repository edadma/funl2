package xyz.hyperreal.funl2

import org.scalatest._
import prop.PropertyChecks

import xyz.hyperreal.bvm._


class Tests extends FreeSpec with PropertyChecks with Matchers {

	"named capture groups" in {
		Pattern.compile( """^(?<a>[a-z]+)-(?<n>[0-9]+)$""" ).allMatches( """xyzzy-14""" ) shouldBe
			Stream(
				Map(
					"0" -> (0, 8, "xyzzy-14"),
					"a" -> (0, 5, "xyzzy"),
					"n" -> (6, 8, "14")))
	}

}