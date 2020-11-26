package xyz.hyperreal.funl2

import scala.util.Using

object Main extends App {

  if (args.isEmpty) {
    println("expected source file")
    sys.exit(1)
  } else
    run(Using(io.Source.fromFile(args(0)))(_.mkString).get,
        Map(),
        Map(),
        args.tail) match {
      case ()     =>
      case result => println(s"Result: $result")
    }

}
