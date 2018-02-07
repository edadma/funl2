package xyz.hyperreal.funl2

import org.scalatest._
import prop.PropertyChecks


class FunLTests extends FreeSpec with PropertyChecks with Matchers {

	"named pattern" in {
		runCapture(
			"""
				|for x@(a, b) <- [(3, 4), (5, 6)]
				|  write( x, a, b )
			""".stripMargin
		) shouldBe
			"""
				|(3, 4), 3, 4
				|(5, 6), 5, 6
			""".stripMargin.trim
	}

	"anonymous functions" in {
		runCapture(
			"""
				|val a = 5
				|val f = x -> x + a
				|
				|def g( fn, x ) = fn( x )
				|
				|write( g(f, 3) )
			""".stripMargin
		) shouldBe "8"
	}

	"anonymous functions (currying 1)" in {
		runCapture(
			"""
				|multiply = (n, m) -> (n * m)
				|write( multiply(3, 4) )
				|
				|curryedMultiply = n -> m -> multiply( n, m )
				|triple = curryedMultiply( 3 )
				|write( triple(4) )
			""".stripMargin
		) shouldBe
			"""
				|12
				|12
			""".stripMargin.trim
	}

	"anonymous functions (currying 2)" in {
		runCapture(
			"""
				|curry = f -> a -> b -> f(a, b)
				|uncurry = f -> (a, b) -> f(a)(b)
				|
				|add = (a, b) -> a + b
				|curriedAdd = a -> b -> a + b
				|
				|write( add(5,6) )
				|write( curriedAdd(5)(6) )
				|
				|write( uncurry(curry(add))(5,6) )
				|write( curry(uncurry(curriedAdd))(5)(6) )
			""".stripMargin
		) shouldBe
			"""
				|11
				|11
				|11
				|11
			""".stripMargin.trim
	}

	"closure (single parameter)" in {
		runCapture(
			"""
				|def f( x )
				|  def g( n ) = x + n
				|
				|  g( 5 ) + 1
				|
				|write( f(3) )
			""".stripMargin
		) shouldBe "9"
	}

	"closure (single parameter, accessing out of scope parameter)" in {
		runCapture(
			"""
				|def f( x )
				|  def g( n ) = x + n
				|
				|  g
				|
				|write( f(3)(4) )
			""".stripMargin
		) shouldBe "7"
	}

	"closure (multi parameter)" in {
		runCapture(
			"""
				|def f( x, y )
				|  def g( m, n ) = x + y + m + n
				|
				|  g( 5, 6 ) + 1
				|
				|write( f(3, 4) )
			""".stripMargin
		) shouldBe "19"
	}

	"closure (with locals)" in {
		runCapture(
			"""
				|def f( x, y )
				|  val (u, v) = (7, 8)
				|
				|  def g( m, n ) = m + n + u + v + x + y
				|
				|  g( 5, 6 ) + 3u + 4v
				|
				|write( f(3, 4) )
			""".stripMargin
		) shouldBe "86"
	}

	"if/elif/else" in {
		runCapture(
			"""
				|def f( n ) = if n == 1 then 'a' elif n == 2 then 'b' else 'c'
				|
				|write( [f(k) | k <- 1..3] )
				|
				|if 3 < 5
				|  write( 1 )
				|elif 4 < 5
				|  write( 2 )
				|
				|write( 3 )
			""".stripMargin
		) shouldBe
		"""
			|["a", "b", "c"]
			|1
			|3
		""".stripMargin.trim
	}

	"defined" in {
		runCapture(
			"""
				|var a = 3
				|var b
				|
				|\a = 123
				|\b = 123
				|
				|write( a, b )
			""".stripMargin
		) shouldBe "123, undefined"
	}

	"undefined" in {
		runCapture(
			"""
				|var a = 3
				|var b
				|
				|/a = 123
				|/b = 123
				|
				|write( a, b )
			""".stripMargin
		) shouldBe "3, 123"
	}

	"set comprehension" in {
		runCapture(
			"""
				|write( {x\2 | x <- 1..5} )
			""".stripMargin
		) shouldBe "{0, 1, 2}"
	}

	"nested every" in {
		runCapture(
			"""
				|every x = 1|2 do
				|  every y = 3|4 do
				|    write( x, y )
			""".stripMargin
		) shouldBe
			"""
				|1, 3
				|1, 4
				|2, 3
				|2, 4
			""".stripMargin.trim
	}

	"nested for" in {
		runCapture(
			"""
				|for x <- [3, 4], y <- [5, 6] do
				|  write( x, y )
			""".stripMargin
		) shouldBe
			"""
				|3, 5
				|3, 6
				|4, 5
				|4, 6
			""".stripMargin.trim
	}

	"mutable maps" in {
		runCapture(
			"""
				|val m = table( {asdf: 3, rtyu: 4} )
				|
				|write( m )
				|m.asdf = 5
				|write( m )
				|m('rtyu') = 6
				|write( m )
			""".stripMargin
		) shouldBe
			"""
				|{"asdf": 3, "rtyu": 4}
				|{"asdf": 5, "rtyu": 4}
				|{"asdf": 5, "rtyu": 6}
			""".stripMargin.trim
	}

	"variable shadowing" in {
		runCapture(
			"""
				|var k = 123
				|
				|if true
				|  var k = 456
				|
				|  write( k )
				|
				|write( k )
			""".stripMargin
		) shouldBe
			"""
				|456
				|123
			""".stripMargin.trim
	}

	"while loop lexical scope" in {
		runCapture(
			"""
				|var n = 0
				|
				|while n++ < 2
				|  val a = 2n
				|  var b = 3n
				|
				|  write( a, b, n )
				|
				|write( a, b, n )
			""".stripMargin
		) shouldBe
			"""
				|2, 3, 1
				|4, 6, 2
				|undefined, undefined, 3
			""".stripMargin.trim
	}

	"break" in {
		runCapture(
			"""
				|var i
				|var j
				|val b = buffer()
				|
				|outer: every i = 1 to 10
				|  every j = 1 to 2
				|    if 3 div i and j == 2 then break
				|    if 5 div i then break outer
				|
				|    b += (i, j)
				|
				|write( b )
				|
				|;;;;;;;;;;;;;;;;
				|
				|i = 0
				|val b1 = buffer()
				|
				|outer: while i++ < 10
				|  j = 0
				|
				|  while j++ < 2
				|    if 3 div i and j == 2 then break
				|    if 5 div i then break outer
				|
 				|    b1 += (i, j)
				|
				|write( b1 )
				|
				|;;;;;;;;;;;;;;;;
				|
				|i = 0
				|val b2 = buffer()
				|
				|outer: repeat
				|  if not i++ < 10 then break
				|  j = 0
				|
				|  repeat
				|    if j++ >= 2 then break
				|    if 3 div i and j == 2 then break
				|    if 5 div i then break outer
				|
				|    b2 += (i, j)
				|
				|write( b2 )
			""".stripMargin
		) shouldBe
			"""
				|ArrayBuffer((1, 1), (1, 2), (2, 1), (2, 2), (3, 1), (4, 1), (4, 2))
				|ArrayBuffer((1, 1), (1, 2), (2, 1), (2, 2), (3, 1), (4, 1), (4, 2))
				|ArrayBuffer((1, 1), (1, 2), (2, 1), (2, 2), (3, 1), (4, 1), (4, 2))
			""".stripMargin.trim
	}

	"continue" in {
		runCapture(
			"""
				|var i
				|var j
				|val b = buffer()
				|
				|outer: every i = 1 to 10
				|  every j = 1 to 2
				|    if 3 div i and j == 2 then continue
				|    if 5 div i then continue outer
				|
				|    b += (i, j)
				|
				|write( b )
				|
				|;;;;;;;;;;;;;;;;
				|
				|i = 0
				|val b1 = buffer()
				|
				|outer: while i++ < 10
				|  j = 0
				|
				|  while j++ < 2
				|    if 3 div i and j == 2 then continue
				|    if 5 div i then continue outer
				|
 				|    b1 += (i, j)
				|
				|write( b1 )
				|
				|;;;;;;;;;;;;;;;;
				|
				|i = 0
				|val b2 = buffer()
				|
				|outer: repeat
				|  if not i++ < 10 then break
				|  j = 0
				|
				|  repeat
				|    if j++ >= 2 then break
				|    if 3 div i and j == 2 then continue
				|    if 5 div i then continue outer
				|
				|    b2 += (i, j)
				|
				|write( b2 )
			""".stripMargin
		) shouldBe
			"""
				|ArrayBuffer((1, 1), (1, 2), (2, 1), (2, 2), (3, 1), (4, 1), (4, 2), (6, 1), (7, 1), (7, 2), (8, 1), (8, 2), (9, 1))
				|ArrayBuffer((1, 1), (1, 2), (2, 1), (2, 2), (3, 1), (4, 1), (4, 2), (6, 1), (7, 1), (7, 2), (8, 1), (8, 2), (9, 1))
				|ArrayBuffer((1, 1), (1, 2), (2, 1), (2, 2), (3, 1), (4, 1), (4, 2), (6, 1), (7, 1), (7, 2), (8, 1), (8, 2), (9, 1))
			""".stripMargin.trim
	}

	"to" in {
		runCapture(
			"""
				|every write( (k = 1 to 3) to k + 2 )
			""".stripMargin
		) shouldBe
			"""
				|1
				|2
				|3
				|2
				|3
				|4
				|3
				|4
				|5
			""".stripMargin.trim
	}

	"elements" in {
		runCapture(
			"""
				|data record( a, b )
				|val r = record( 123, 456 )
				|val m = {a: 123, "b": 456}
				|
				|write( r.a, r.b, r, r("b"), r(1) )
				|write( m.a, m.b, m, m("a") )
				|write( [3, 4, 5](1) )
			""".stripMargin
		) shouldBe
			"""
				|123, 456, record( 123, 456 ), 456, 456
				|123, 456, {"a": 123, "b": 456}, 123
				|4
			""".stripMargin.trim
	}

	"repeated alternation 1" in {
		runCapture(
			"""
				|var a = 0
				|
				|every write( |(if a++ < 3 then a) )
			""".stripMargin
		) shouldBe
			"""
				|1
				|2
				|3
			""".stripMargin.trim
	}

	"list comprehension scope" in {
		runCapture(
			"""
				|var k = 123
				|
				|write( [2k - 1 | k <- 1..<3] )
				|
				|write( k )
			""".stripMargin
		) shouldBe
			"""
				|[1, 3]
				|123
			""".stripMargin.trim
	}

	"for loop scope" in {
		runCapture(
			"""
				|val a = 123
				|
				|for a <- 1..3
				|  write( a )
				|
				|write( a )
			""".stripMargin
		) shouldBe
			"""
				|1
				|2
				|3
				|123
			""".stripMargin.trim
	}

	"nested block declaration" in {
		runCapture(
			"""
				|data num( asdf )
				|
				|val (fac4, constructor) =
				|  data num( v )
				|
				|  var x = num( 1 )
				|
				|  for i <- 1..4 do x = num( x.v*i )
				|
				|  (x.v, num)
				|
				|write( fac4, constructor, x, num )
			""".stripMargin
		) shouldBe "24, RecordConstructor(num, num, ['v]), undefined, RecordConstructor(num, num, ['asdf])"
	}

	"nested compound expression declaration" in {
		runCapture(
			"""
				|val fac4 = (var x = 1; for i <- 1..4 do (write( i ); x *= i); x)
				|
				|write( fac4, x )
			""".stripMargin
		) shouldBe
		"""
			|1
			|2
			|3
			|4
			|24, undefined
		""".stripMargin.trim
	}

}