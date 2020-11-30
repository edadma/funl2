//@
package xyz.hyperreal.funl2

import xyz.hyperreal.bvm._

import scala.collection.immutable.ArraySeq

object Main extends App {
  val program =
//    """
//     |def f( a )
//     |  def g( b ) = a + b
//     |
//     |  h( g, 4 )
//     |
//     |def h(fn, x) = fn( x )
//     |
//     |write( f(3) )
// 		""".stripMargin
    """
      |var c
			|
			|"aasdf" ? c = (capture("g1", rep(string("a"))))
			|
			|write(c)
    """.stripMargin

  val parser = new FunLParser
  val ast = parser.parseFromString(program, parser.source).asInstanceOf[AST]
  val compiler = new Compiler(Predef.constants ++ Predef.natives,
                              Predef.sysvars,
                              Predef.macros,
                              comments = true)
  val code = compiler.compile(ast)
  val vm = new VM(code, ArraySeq(), false, true, args)

//	println( code.functions, code.variables )
  println(ast)
//	println( code.code.zipWithIndex map {case (c, i) => s"$i\t$c"} mkString "\n" )
//	vm.trace = true
//	vm.limit = 300
  vm.execute
//	println( vm )
}

//todo: 'a' can't be accessed from 'new AnyRef {def a( x: Int ) = 123}'
//todo: function overloading
//todo: method invocation where parentheses are not needed for zero argument methods

//todo: digits wasn't seen as a character class
//todo: most built-in constants should start with & (as in Icon)
//todo: regex not working
//todo: size() method on Map types not working

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
