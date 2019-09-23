//@
package xyz.hyperreal.funl2

import xyz.hyperreal.bvm._


object Main extends App {
	val program =
		"""
     |write( 'hello world' )
 		""".stripMargin

	val parser = new FunLParser
	val ast = parser.parseFromString( program, parser.source ).asInstanceOf[AST]
	val compiler = new Compiler( Predef.constants ++ Predef.natives, Predef.sysvars, Predef.macros, comments = true )
	val code = compiler.compile( ast )
	val vm = new VM( code, Array(), false, true, new AnyRef {def a( x: Int ) = 123} )

//	println( code.functions, code.variables )
//	println( ast )
//	println( code.code.zipWithIndex map {case (c, i) => i + "\t" + c} mkString "\n" )
//	vm.trace = true
//	vm.limit = 300
//	println( vm.call( 2, List(3) ) )
	vm.execute
	println( vm )
//	println( vm.call(vm.global(0), List(5, 7)) )
//	println( vm.call(code.constants("array"), List(3)) )
}

//todo: 'a' can't be accessed from 'new AnyRef {def a( x: Int ) = 123}'
//todo: function overloading
//todo: method invocation where parentheses are not needed for zero argument methods

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
