package info.necauqua.scalabf

import java.io.FileNotFoundException

import scala.io.{Source, StdIn}

object Start extends App {

  import Interpreter._

  def error(err: String, code: Int): Nothing = {
    Console.err.println(err)
    sys.exit(code)
  }

  def getSrc(file: String): String =
    try {
      Source.fromFile(file).mkString
    }catch {
      case _: FileNotFoundException =>
        error(s"File $file was not found!", 2)
    }

  def repl(): Unit = {
    val memory = new Memory
    var line = StdIn.readLine()
    while(line != "stop") {
      if(!syntaxCheck(line)) { // check for braces with syntaxCheck, heh
        line += StdIn.readLine()
      }else {
        run(line, memory)
        line = StdIn.readLine()
      }
    }
  }

  def run(str: String, memory: Memory = new Memory): Unit =
    if(syntaxCheck(str)) {
      optimize(parse(str)).foreach(_.apply(memory))
    }else {
      error("error: Braces do not match!", 1)
    }

  def check(str: String): Unit = {
    val check = syntaxCheck(str)
    println("syntax: " + check)
    if(check) {
      val parsed = parse(str)
      println("parsed:")
      println(prettyPrint(parsed))
      val optimized = optimize(parsed)
      println("optimized:")
      println(prettyPrint(optimized))
    }
  }

  args match {
    case Array() | Array("-i") => repl()
    case Array("-c", src) => run(src)
    case Array("-f", file) => run(getSrc(file))
    case Array("-p", file) => check(getSrc(file))
    case Array("-pc", src) => check(src)
    case _ => println(s"Wrong arguments!") // lazy
  }
}
