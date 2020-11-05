package xyz.hyperreal.funl2

import xyz.hyperreal.bvm._

import scala.collection.immutable.ArraySeq


object Main extends App {

  if (args.isEmpty) {
    println("expected source file")
    sys.exit(1)
  } else {
    val parser = new FunLParser
    val ast = parser.parseFromSource(io.Source.fromFile(args.head), parser.source).asInstanceOf[AST]
    val compiler = new Compiler(Predef.constants ++ Predef.natives, Predef.sysvars, Predef.macros, comments = true)
    val code = compiler.compile(ast)
    val vm = new VM(code, ArraySeq(), false, true, args.tail)

    vm.execute match {
      case () =>
      case result => println(s"Result: $result")
    }
  }

}