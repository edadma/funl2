package xyz.hyperreal.funl2

import org.scalatest._
import prop.PropertyChecks


class FunLExamples extends FreeSpec with PropertyChecks with Matchers {

	"hanoi" in {
		runCapture(
			"""
				|val a = buffer( 5..1 by -1 )
				|val b = buffer()
				|val c = buffer()
				|
				|def hanoi( n, source, target, auxilliary )
				|  if n > 0
				|    ;; move n - 1 disks from source to auxiliary, so they are out of the way
				|    hanoi( n - 1, source, auxilliary, target )
				|
				|    ;; move the nth disk from source to target
				|    target += source.remove( source.length() - 1 )
				|
				|    ;; move the n - 1 disks that we left on auxiliary onto target
				|    hanoi( n - 1, auxilliary, target, source )
				|
				|hanoi( 5, a, b, c )
				|write( a, b, c )
			""".stripMargin
		) shouldBe "ArrayBuffer(), ArrayBuffer(5, 4, 3, 2, 1), ArrayBuffer()"
	}

	"bmi 1" in {
		runCapture(
			"""
				|def bmiTell( weight, height )
				|  | bmi <= 18.5 = "You're underweight, you emo, you!"
				|  | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
				|  | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
				|  | otherwise   = "You're a whale, congratulations!"
				|  where bmi = weight / height ^ 2
				|
				|write( bmiTell(95.2544, 1.675) )
				|write( bmiTell(70, 1.675) )
			""".stripMargin
		) shouldBe
			"""
				|You're a whale, congratulations!
				|You're supposedly normal. Pffft, I bet you're ugly!
			""".stripMargin.trim
	}

	"bmi 2" in {
		runCapture(
			"""
				|def bmiTell( weight, height )
				|  | bmi <= skinny = "You're underweight, you emo, you!"
				|  | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
				|  | bmi <= fat = "You're fat! Lose some weight, fatty!"
				|  | otherwise   = "You're a whale, congratulations!"
				|  where
				|    bmi = weight / height ^ 2
				|    (skinny, normal, fat) = (18.5, 25.0, 30.0)
				|
				|write( bmiTell(95.2544, 1.675) )
				|write( bmiTell(70, 1.675) )
			""".stripMargin
		) shouldBe
			"""
				|You're a whale, congratulations!
				|You're supposedly normal. Pffft, I bet you're ugly!
			""".stripMargin.trim
	}

	"bmi 3" in {
		runCapture(
			"""
				|def calcBmis( xs ) = [bmi( w, h ) | (w, h) <- xs] where bmi( weight, height ) = weight / height ^ 2
				|
				|write( calcBmis([(95.2544, 1.675), (70, 1.675)]) )
			""".stripMargin
		) shouldBe "[33.95122298952996, 24.94987747828024]"
	}

	"bmi 4" in {
		runCapture(
			"""
				|def calcBmis( xs ) = [bmi( w, h ) | (w, h) <- xs]
				|  where bmi( weight, height ) = weight / height ^ 2
				|
				|write( calcBmis([(95.2544, 1.675), (70, 1.675)]) )
			""".stripMargin
		) shouldBe "[33.95122298952996, 24.94987747828024]"
	}

	"bmi 5" in {
		runCapture(
			"""
				|def calcBmis( xs ) = [bmi( w, h ) | (w, h) <- xs]
				|  where
				|    bmi( weight, height ) = weight / height ^ 2
				|
				|write( calcBmis([(95.2544, 1.675), (70, 1.675)]) )
			""".stripMargin
		) shouldBe "[33.95122298952996, 24.94987747828024]"
	}

	"divisors" in {
		runCapture(
			"""
				|def divisors( n )
				|  var dlist = []
				|
				|  val cr =
				|    for d <- 1..
				|      if d*d >= n then break (d)
				|
				|      if d div n
				|        dlist += d
				|        yield d
				|
				|  if cr*cr == n then yield cr
				|
				|  n/!dlist
				|
				|var divs = buffer()
				|
				|every divs += divisors( 12 )
				|write( divs )
			""".stripMargin
		) shouldBe "ArrayBuffer(1, 2, 3, 12, 6, 4)"
	}

	"exponential 1 (rewritten from Haskell)" in {
		runCapture(
			"""
				|def pow( _, 0 ) = 1
				|def pow( x, n ) | n > 0 = pow_( x, n - 1, x )
				|  where
				|    pow_( _, 0, v ) = v
				|    pow_( u, n, v )
				|      | 2 div n = pow_( u*u, n/2, v )
				|      | otherwise = pow_( u, n - 1, u*v )
				|def pow( _, _ ) = error( "pow: negative exponent" )
				|
				|for n <- 0..18
				|  write( n, pow(3, n) )
			""".stripMargin
		) shouldBe
			"""
				|0, 1
				|1, 3
				|2, 9
				|3, 27
				|4, 81
				|5, 243
				|6, 729
				|7, 2187
				|8, 6561
				|9, 19683
				|10, 59049
				|11, 177147
				|12, 531441
				|13, 1594323
				|14, 4782969
				|15, 14348907
				|16, 43046721
				|17, 129140163
				|18, 387420489
			""".stripMargin.trim
	}

	"exponential 2 (from Haskell)" in {
		runCapture(
			"""
				|def pow( _, 0 ) = 1
				|def pow( x, n ) | n > 0 = f( x, n - 1, x )
				|  where
				|    f( _, 0, y ) = y
				|    f( x, n, y ) = g( x, n )
				|      where
				|        g( x, n )
				|          | 2 div n = g( x*x, n/2 )
				|          | otherwise = f( x, n - 1, x*y )
				|def pow( _, _ ) = error( "pow: negative exponent" )
				|
				|for n <- 0..18
				|  write( n, pow(2, n) )
				|for n <- 0..18
				|  write( n, pow(3, n) )
			""".stripMargin
		) shouldBe
			"""
				|0, 1
				|1, 2
				|2, 4
				|3, 8
				|4, 16
				|5, 32
				|6, 64
				|7, 128
				|8, 256
				|9, 512
				|10, 1024
				|11, 2048
				|12, 4096
				|13, 8192
				|14, 16384
				|15, 32768
				|16, 65536
				|17, 131072
				|18, 262144
				|0, 1
				|1, 3
				|2, 9
				|3, 27
				|4, 81
				|5, 243
				|6, 729
				|7, 2187
				|8, 6561
				|9, 19683
				|10, 59049
				|11, 177147
				|12, 531441
				|13, 1594323
				|14, 4782969
				|15, 14348907
				|16, 43046721
				|17, 129140163
				|18, 387420489
			""".stripMargin.trim
	}

	"exponential 3 (rewritten from Haskell; with mutually recursive 'where' definitions)" in {
		runCapture(
			"""
				|def pow( _, 0 ) = 1
				|def pow( x, n ) | n > 0 = pow_( x, n - 1, x )
				|  where
				|    pow_( _, 0, v ) = v
				|    pow_( u, n, v )
				|      | even( n ) = pow_( u*u, n/2, v )
				|      | otherwise = pow_( u, n - 1, u*v )
				|    even( 0 ) = true
				|    even( n ) = odd( n - 1 )
				|    odd( 0 ) = fail
				|    odd( n ) = even( n - 1 )
				|def pow( _, _ ) = error( "pow: negative exponent" )
				|
				|for n <- 0..18
				|  write( n, pow(3, n) )
			""".stripMargin
		) shouldBe
			"""
				|0, 1
				|1, 3
				|2, 9
				|3, 27
				|4, 81
				|5, 243
				|6, 729
				|7, 2187
				|8, 6561
				|9, 19683
				|10, 59049
				|11, 177147
				|12, 531441
				|13, 1594323
				|14, 4782969
				|15, 14348907
				|16, 43046721
				|17, 129140163
				|18, 387420489
			""".stripMargin.trim
	}

	"n queens 1" in {
		runCapture(
			"""
				|val n = 4
				|
				|var solution = array( n )
				|var up = array( 2n - 1 )
				|var down = array( 2n - 1 )
				|var row = array( n )
				|
				|def solve( c )
				|  every row(r = 0 until n) == down(r + c) == up(n - 1 + r - c) == undefined and row(r) <- down(r + c) <- up(n - 1 + r - c) <- r
				|    solution(c) = r
				|
				|    if c == n - 1
				|      write( seq(solution) )
				|    else
				|      solve( c + 1 )
				|
				|solve( 0 )
			""".stripMargin
		) shouldBe
			"""
				|<1, 3, 0, 2>
				|<2, 0, 3, 1>
			""".stripMargin.trim
	}

	"n queens 2" in {
		runCapture(
			"""
				|def
				|  permute( [] ) = seq()
				|  permute( l ) = permute_( l.length(), array(l) )
				|  permute_( n, a )
				|    if n == 1
				|      seq( a )
				|    else
				|      for i <- 0..<n - 1
				|        yield permute_( n - 1, a )
				|
				|        if 2 div n
				|          swap( a(i), a(n - 1) )
				|        else
				|          swap( a(0), a(n - 1) )
				|
				|      permute_( n - 1, a )
				|
				|val n = 4
				|
				|var up = array( 2n - 1 )
				|var down = array( 2n - 1 )
				|
				|def place( p, c ) | c == n = write( p )
				|def place( p, c )
				|  every down(p(c) + c) == up(n - 1 + p(c) - c) == undefined and down(p(c) + c) <- up(n - 1 + p(c) - c) <- 1
				|    place( p, c + 1 )
				|
				|every place( permute(0..n - 1), 0 )
			""".stripMargin
		) shouldBe
			"""
				|<1, 3, 0, 2>
				|<2, 0, 3, 1>
			""".stripMargin.trim
	}

	"sum 3,5" in {
		runCapture(
			"""
				|def
				|  foldl( f, z, [] )           = z
				|  foldl( f, z, x:xs )         = foldl( f, f(z, x), xs )
				|
				|  sum( l ) = foldl( (+), 0, l )
				|
				|write( sum([n | n <- 1..1000-1 if 3 div n or 5 div n]) )
			""".stripMargin
		) shouldBe "233168"
	}

	"pythagorean triples" in {
		runCapture(
			"""
				|def triples( n ) = [(a, b, c) | a <- 1..n-2, b <- a+1..n-1, c <- b+1..n if a^2 + b^2 == c^2]
				|
				|write( triples(20) )
			""".stripMargin
		) shouldBe "[(3, 4, 5), (5, 12, 13), (6, 8, 10), (8, 15, 17), (9, 12, 15), (12, 16, 20)]"
	}

	"seriesUp" in {
		runCapture(
			"""
				|def
				|  foldr( f, z, [] )     =  z
				|  foldr( f, z, (x:xs) ) =  f( x, foldr(f, z, xs) )
				|
				|  map( f, [] )                = []
				|  map( f, x:xs )              = f( x ) : map( f, xs )
				|
				|  concat( xss ) = foldr( (+), [], xss )
				|
				|  concatMap( f, xss ) = concat( map(f, xss) )
				|
				|def seriesUp( n ) = concatMap( n -> 1..n, 1..n ).toList()
				|
				|write( seriesUp(4) )
			""".stripMargin
		) shouldBe "[1, 1, 2, 1, 2, 3, 1, 2, 3, 4]"
	}

	"merge sort" in {
		runCapture(
			"""
				|def
				|  merge( [], ys )               = ys
				|  merge( xs, [] )               = xs
				|  merge( xs@(x:xt), ys@(y:yt) )
				|    | x <= y                    = x : merge( xt, ys )
				|    | otherwise                 = y : merge( xs, yt )
				|
				|  split( (x:y:zs) ) = (x:xs, y:ys) where (xs, ys) = split( zs )
				|  split( [x] )      = ([x], [])
				|  split( [] )       = ([], [])
				|
				|  mergeSort( [] )  = []
				|  mergeSort( [x] ) = [x]
				|  mergeSort( xs )  = merge( mergeSort(as), mergeSort(bs) ) where (as,bs) = split( xs )
				|
				|write( mergeSort([4, 6, 9, 1, 3, 5, 7, 2, 8]) )
			""".stripMargin
		) shouldBe "[1, 2, 3, 4, 5, 6, 7, 8, 9]"
	}

	"quicksort 1" in {
		runCapture(
			"""
				|def
				|  quicksort( [] )    =  []
				|  quicksort( x:xs )  =  quicksort( [a | a <- xs if a <= x] ) + [x] + quicksort( [a | a <- xs if a > x] )
				|
				|write( quicksort([4, 6, 9, 1, 3, 5, 7, 2, 8]) )
			""".stripMargin
		) shouldBe "[1, 2, 3, 4, 5, 6, 7, 8, 9]"
	}

	"quicksort 2" in {
		runCapture(
			"""
				|def foldr( f, z, [] )     =  z
				|def foldr( f, z, (x:xs) ) =  f( x, foldr(f, z, xs) )
				|
				|val select = p -> (x, (ts,fs)) -> if p( x ) then (x:ts,fs) else (ts, x:fs)
				|
				|def partition( p, xs ) = foldr( select(p), ([],[]), xs )
				|
				|def
				|  qsort( [] )     = []
				|  qsort( (x:xs) ) = qsort( ys ) + (x : qsort(zs))
				|    where
				|      (ys, zs) = partition( (< x), xs )
				|
				|write( qsort([4, 6, 9, 1, 3, 5, 7, 2, 8]) )
			""".stripMargin
		) shouldBe "[1, 2, 3, 4, 5, 6, 7, 8, 9]"
	}

	"filter" in {
		runCapture(
			"""
				|def
				|  foldl( f, z, [] )           = z
				|  foldl( f, z, x:xs )         = foldl( f, f(z, x), xs )
				|
				|  map( f, [] )                = []
				|  map( f, x:xs )              = f( x ) : map( f, xs )
				|
				|  filter( p, [] )             = []
				|  filter( p, x:xs ) | p( x )  = x : filter( p, xs )
				|  filter( p, _:xs )           = filter( p, xs )
				|
				|write( foldl((+), 0, map((*2), filter((4<), 3..6))) )
			""".stripMargin
		) shouldBe "22"
	}

	"fibonacci 1" in {
		runCapture(
			"""
				|def fib( n )
				|  result = []
				|  a, b = 0, 1
				|
				|  while a < n
				|    result += a
				|    a, b = b, a + b
				|
				|  result
				|
				|write( fib(10) )
			""".stripMargin
		) shouldBe "[0, 1, 1, 2, 3, 5, 8]"
	}

	"factorial 1" in {
		runCapture(
			"""
				|def
				|  factorial( 0 | 1 ) = 1
				|  factorial( n ) = n*factorial( n - 1 )
				|
				|write( factorial(5) )
			""".stripMargin
		) shouldBe "120"
	}

	"factorial 2" in {
		runCapture(
			"""
				|def factorial( n )
				|  var i = 1
				|
				|  every i *= 1 to n
				|  i
				|
				|write( factorial(5) )
			""".stripMargin
		) shouldBe "120"
	}

	"crossword" in {
		runCapture(
			"""
				|def cross( word1, word2 )
				|  var i
				|  var j
				|
				|  every i = upto( word2, word1 )
				|    every j = upto( word1[i], word2 )
				|      every write( right(word2[1 to j - 1], i) )
				|      write( word1 )
				|      every write( right(word2[j + 1 to word2.length()], i) )
				|      write()
				|
				|cross( 'lottery', 'loto' )
			""".stripMargin
		) shouldBe
			"""
				|lottery
				|o
				|t
				|o
				|
				| l
				|lottery
				| t
				| o
				|
				| l
				| o
				| t
				|lottery
				|
				|  l
				|  o
				|lottery
				|  o
				|
				|   l
				|   o
				|lottery
				|   o
			""".stripMargin.trim
	}

	"ackermann" in {
		runCapture(
			"""
				|def
				|  ackermann( 0, n ) = n + 1
				|  ackermann( m, 0 ) = ackermann( m - 1, 1 )
				|  ackermann( m, n ) = ackermann( m - 1, ackermann(m, n - 1) )
				|
				|for m <- 0..3, n <- 0..4
				|  write( "Ackermann( $m, $n ) = " + ackermann(m, n) )
			""".stripMargin
		) shouldBe
			"""
				|Ackermann( 0, 0 ) = 1
				|Ackermann( 0, 1 ) = 2
				|Ackermann( 0, 2 ) = 3
				|Ackermann( 0, 3 ) = 4
				|Ackermann( 0, 4 ) = 5
				|Ackermann( 1, 0 ) = 2
				|Ackermann( 1, 1 ) = 3
				|Ackermann( 1, 2 ) = 4
				|Ackermann( 1, 3 ) = 5
				|Ackermann( 1, 4 ) = 6
				|Ackermann( 2, 0 ) = 3
				|Ackermann( 2, 1 ) = 5
				|Ackermann( 2, 2 ) = 7
				|Ackermann( 2, 3 ) = 9
				|Ackermann( 2, 4 ) = 11
				|Ackermann( 3, 0 ) = 5
				|Ackermann( 3, 1 ) = 13
				|Ackermann( 3, 2 ) = 29
				|Ackermann( 3, 3 ) = 61
				|Ackermann( 3, 4 ) = 125
			""".stripMargin.trim
	}

	"perfect" in {
		runCapture(
			"""
				|def
				|  foldl( f, z, [] )           = z
				|  foldl( f, z, x:xs )         = foldl( f, f(z, x), xs )
				|
 				|  sum( l ) = foldl( (+), 0, l )
				|
				|  perfect( n ) = sum( [d | d <- 1..n if d div n] ) == 2n
				|
				|for i <- 1..30 if perfect( i )
				|   write( i )
			""".stripMargin
		) shouldBe
			"""
				|6
				|28
			""".stripMargin.trim
	}

	"gcd 1" in {
		runCapture(
			"""
				|def gcd( a, b )
				|  var a_ = a
				|  var b_ = b
				|
				|  while (r = a_ mod b_) != 0
				|     a_, b_ = b_, r
				|
				|  b_
				|
				|write( gcd(30, 12) )
			""".stripMargin
		) shouldBe "6"
	}

	"gcd 2" in {
		runCapture(
			"""
				|def
				|  gcd( a, a )   = a
				|  gcd( a, b )
				|    | a > b     = gcd( a - b, b )
				|    | otherwise = gcd( a, b - a )
				|
				|write( gcd(30, 12) )
			""".stripMargin
		) shouldBe "6"
	}

	"gcd 3" in {
		runCapture(
			"""
				|def gcd( 0, 0 ) = error( "gcd: gcd( 0, 0 ) is undefined" )
				|def gcd( x, y ) = gcd_( abs(x), abs(y) )
				|  where
				|    gcd_( x, 0 ) = x
				|    gcd_( x, y ) = gcd_( y, x mod y )
				|
				|write( gcd(1, 1) )
				|write( gcd(1, 3) )
				|write( gcd(1, 4) )
				|write( gcd(3, 3) )
				|write( gcd(4, 4) )
				|write( gcd(5, 7) )
				|write( gcd(5, 6) )
				|write( gcd(6, 8) )
				|write( gcd(12, 8) )
				|write( gcd(12, 30) )
				|write( gcd(30, 12) )
			""".stripMargin
		) shouldBe
			"""
				|1
				|1
				|1
				|3
				|4
				|1
				|1
				|2
				|4
				|6
				|6
			""".stripMargin.trim
	}

	"max 1" in {
		runCapture(
			"""
				|def
				|  max( [] ) = error( "max: empty list" )
				|  max( h:t )
				|    var maximum = h
				|
				|    every maximum <:= !t
				|
				|    maximum
				|
				|every write( max([1, 2, 3, 4, 0, -1]) )
			""".stripMargin
		) shouldBe "4"
	}

	"sign" in {
		runCapture(
			"""
				|def
				|  sign( x )
				|    | x < 0     = -1
				|    | x == 0    = 0
				|    | otherwise = 1
				|
				|write( sign(2) )
			""".stripMargin
		) shouldBe "1"
	}

	"take" in {
		runCapture(
			"""
				|def
				|  filter( p, [] )             =  []
				|  filter( p, x:xs ) | p( x )  =  x : filter( p, xs )
				|  filter( p, _:xs )           =  filter( p, xs )
				|
				|  take( 0, _ )          =  []
				|  take( _, [] )         =  []
				|  take( n, _ ) | n < 0  =  error( "take: expected non-negative integer" )
				|  take( n, x:xs )       =  x : take( n - 1, xs )
				|
				|write( take(2, filter((>= 5), 1..10)) )
				|write( take(2, filter(a -> a >= 5, 1..10)) )
			""".stripMargin
		) shouldBe
			"""
				|[5, 6]
				|[5, 6]
			""".stripMargin.trim
	}

	"flatten" in {
		runCapture(
			"""
				|def flatten( l )
				|  val res = buffer()
				|
				|  for x <- l
				|    if type( x ) == "iterable"
				|      res ++= flatten( x )
				|    else
				|      res += x
				|
				|  res.toList()
				|
				|write( flatten([[1], 2, [[3,4], 5], [[[]]], [[[6]]], 7, 8, []]) )
			""".stripMargin
		) shouldBe "[1, 2, 3, 4, 5, 6, 7, 8]"
	}

	"select" in {
		runCapture(
			"""
				|every write( divisors(1001) )
				|
				|def divisors( n ) = select( 1 to n, (div n) )
				|
				|def select( a, p ) = if p( a ) then a
			""".stripMargin
		) shouldBe
			"""
				|1
				|7
				|11
				|13
				|77
				|91
				|143
				|1001
			""".stripMargin.trim
	}

	"string scan 1" in {
		runCapture(
			"""
				|'abcdefg' ?
				|  while move(1)
				|    write( move(1) )
				|
				|'abcdefg' ?
				|  if tab(3)
				|    while move(1)
				|      write( move(1) )
				|
				|'abcdefg' ?
				|  tab(0)
				|  while write( move(-1) )
				|
				|'abcdefg' ?
				|  tab(3)
				|  write( if pos(3) then 'yes' else 'no' )
				|
				|write( 'abc' ? find('b') )
					""".stripMargin
		) shouldBe
			"""
				|b
				|d
				|f
				|d
				|f
				|g
				|f
				|e
				|d
				|c
				|b
				|a
				|yes
				|2
			""".stripMargin.trim
	}

	"string scan 2" in {
		runCapture(
			"""
				|val vowel = cset( 'aeiou' )
				|val text = 'test string'
				|
				|every write( text ? upto(vowel) )
				|
				|text ?
				|  if tab(upto(vowel) + 1)
				|    write(tab(0))
				|
				|text ?
				|  while write(tab(upto(' ')))
				|    tab(many(' '))
				|
				|  write(tab(0))
				|
				|text ?
				|  while tab(upto(alphanum))
				|    write(tab(many(alphanum)))
				|
				|write( "The theory is fallacious" ? match("The") )
				|
				|if "The theory is fallacious" ? match(" theory") then write( 'yes' ) else write( 'no' )
				|
				|write("Our conjecture has support" ? tab(any('aeiouAEIOU')))
				|
				|if not write("Our conjecture has support" ? tab(any('aeiou'))) then write( 'no' )
				|
				|text ?
				|  while tab(upto(alphanum))
				|    tab(many(alphanum)) ?
				|      if upto('aeiou') then write(move(1))
				|
				|var line = '   asdf'
				|
				|line ?= tab(many(' ')) & tab(0)
				|write( line )
			""".stripMargin
		) shouldBe
			"""
				|2
				|9
				|st string
				|test
				|string
				|test
				|string
				|4
				|no
				|O
				|no
				|t
				|s
				|asdf
			""".stripMargin.trim
	}

	"data backtracking 1" in {
		runCapture(
			"""
				|"asdf,zxvc." ? tab(upto(',') + 1) & write(move(1)) | write(tab(upto('.')))
				|"asdf," ? tab(upto(',') + 1) & write(move(1)) | write(tab(upto('.')))
				|"asdf." ? tab(upto(',') + 1) & write(move(1)) | write(tab(upto('.')))
				|"asdf" ? tab(upto(',') + 1) & write(move(1)) | write(tab(upto('.')))
			""".stripMargin
		) shouldBe
			"""
				|z
				|asdf
			""".stripMargin.trim
	}

	"permutations" in {
		runCapture(
			"""
				|every write( permute([1, 2, 3, 4]) )
				|
				|def
				|  permute( [] ) = seq()
				|  permute( l ) = permute_( l.length(), array(l) )
				|  permute_( n, a )
				|    if n == 1
				|      seq( a )
				|    else
				|      for i <- 0..<n - 1
				|        yield permute_( n - 1, a )
				|
				|        if 2 div n
				|          swap( a(i), a(n - 1) )
				|        else
				|          swap( a(0), a(n - 1) )
				|
				|      permute_( n - 1, a )
			""".stripMargin
		) shouldBe
			"""
				|<1, 2, 3, 4>
				|<2, 1, 3, 4>
				|<3, 1, 2, 4>
				|<1, 3, 2, 4>
				|<2, 3, 1, 4>
				|<3, 2, 1, 4>
				|<4, 2, 1, 3>
				|<2, 4, 1, 3>
				|<1, 4, 2, 3>
				|<4, 1, 2, 3>
				|<2, 1, 4, 3>
				|<1, 2, 4, 3>
				|<1, 3, 4, 2>
				|<3, 1, 4, 2>
				|<4, 1, 3, 2>
				|<1, 4, 3, 2>
				|<3, 4, 1, 2>
				|<4, 3, 1, 2>
				|<4, 3, 2, 1>
				|<3, 4, 2, 1>
				|<2, 4, 3, 1>
				|<4, 2, 3, 1>
				|<3, 2, 4, 1>
				|<2, 3, 4, 1>
			""".stripMargin.trim
	}

}