package xyz.hyperreal.funl2

object Main extends App {

  if (args.isEmpty) {
    println("expected source file")
    sys.exit(1)
  } else
    run(args(0), Map(), Map(), args.tail) match {
      case ()     =>
      case result => println(s"Result: $result")
    }

}
