//@
package xyz.hyperreal.funl2

import xyz.hyperreal.bvm._


object Main extends App {
	val program =
//		"""
//			|def levenstein( s1, s2 ) = lev( s1.length(), s2.length(), s1, s2 )
//			|
//			|def lev( i, j, a, b )
//			|  | i | j == 0  =  max( i, j )
//			|  | otherwise   =  min( lev(i - 1, j, a, b) + 1,
//			|                        lev(i, j - 1, a, b) + 1,
//			|                        lev(i - 1, j - 1, a, b) + (if a[i] == b[j] then 0 else 1) )
//			|
//			|write( levenstein('kitten', 'sitting') )
//			|write( levenstein('rosettacode', 'raisethysword') )
// 		""".stripMargin
//		"""
//			|for i <- 1..1200
//			|  val s = sum( divisors(i) )
//			|
//			|  if i < s and i == sum( divisors(s) )
//			|    write( i, s )
//			|
//			|def divisors( n )
//			|  var s = 1
//			|
//			|  for i <- 1..n/2
//			|    if i div n then d += i
//			|
//			|  d
// 		""".stripMargin
//		"""
//			|'asdf' ? write( `(asdf)` )
// 		""".stripMargin

		"""
			|write( abs(1 + i) )
 		""".stripMargin

	val parser = new FunLParser
	val ast = parser.parseFromString( program, parser.source ).asInstanceOf[AST]
	val compiler = new Compiler( Predef.constants ++ Predef.natives, Predef.sysvars, Predef.macros, comments = true )
	val code = compiler.compile( ast )
	val vm = new VM( code, Array(), false, true, null )

//	println( code.functions, code.variables )
//	println( ast )
//	println( code.code.zipWithIndex map {case (c, i) => i + "\t" + c} mkString "\n" )
//	vm.trace = true
//	vm.limit = 300
//	println( vm.call( 2, List(3) ) )
	println( vm.execute )
	println( vm )
//	println( vm.call(vm.global(0), List(5, 7)) )
//	println( vm.call(code.constants("array"), List(3)) )
}

//todo: function overloading
//todo: method invokation where parentheses are not needed for zero argument methods

//		"""
//			|def
//			|  merge( [], ys )               = ys
//			|  merge( xs, [] )               = xs
//			|  merge( xs@(x:xt), ys@(y:yt) )
//			|    | x <= y                    = x : merge( xt, ys )
//			|    | otherwise                 = y : merge( xs, yt )
//			|
//			|  split( (x:y:zs) ) = (x:xs, y:ys) where (xs, ys) = split( zs )
//			|  split( [x] )      = ([x], [])
//			|  split( [] )       = ([], [])
//			|
//			|  mergeSort( [] )  = []
//			|  mergeSort( [x] ) = [x]
//			|  mergeSort( xs )  = merge( mergeSort(as), mergeSort(bs) ) where (as,bs) = split( xs )
//			|
//			|def time( task )
//			|  def perform
//			|    gc()
//			|
//			|    val start = $timemillis
//			|
//			|    task()
//			|    $timemillis - start
//			|
//			|  write( (perform() + perform() + perform())\3 + ' msec' )
//			|
//			|seed( 0 )
//			|
//			|val l = [rnd(0..9) | _ <- 1..1000]
//			|
//			|time( () -> mergeSort(l) )
//		""".stripMargin
