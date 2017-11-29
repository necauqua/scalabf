package info.necauqua.scalabf

object Interpreter {

  /**
    * Checks the string for validly opened and closed `[` and `]` braces.
    * It is the only valid syntax error in brainfuck, hence the name.
    * @param str source code to check
    * @return true if loop braces are paired evenly
    **/
  def syntaxCheck(str: String): Boolean =
    str.collect {
      case '[' => 1
      case ']' => -1
    }.reduceOption { (acc, x) =>
      val sum = acc + x
      if(sum < 0) {
        return false // non-local return from reduce - highly functional
      }
      sum
    }.getOrElse(0) == 0

  /**
    * Parses given code to a list of tokens.
    * Loops are handled recursively, so SOE's my occur with high deepness.
    * @param chars source code to parse
    * @return list of tokens describing the program
    **/
  def parse(chars: Seq[Char]): List[Token] = {

    val iter = chars.iterator

    // this, ming-bendingly, works well
    def rec(limited: Iterator[Char]): List[Token] =
      limited.collect {
        case '>' => Move(1)
        case '<' => Move(-1)
        case '+' => Change(1)
        case '-' => Change(-1)
        case '.' => Write(1)
        case ',' => Read(1)
        case '[' => Loop(rec(iter.takeWhile(_ != ']')))
      }.toList

    rec(iter)
  }

  /**
    * Optimizes given list of tokens by merging them with each other.
    * Also replaces constructs like [-] with Set(0).
    * As with parse, this recursively calls itself on loops.
    * @param tokens list of tokens to be optimized
    * @return optimized list of tokens
    **/
  def optimize(tokens: Seq[Token]): Seq[Token] = {
    tokens.foldRight(List.empty[Token]) { (t, acc) =>
      val next = t match { // recursively handle loops and resets
        case Loop(code) =>
          optimize(code) match {
            case Change(-1) :: Nil => Set(0, 0)
            case Set(0, 0) :: Nil => Set(0, 0)
            case opt => Loop(opt)
          }
        case x => x
      }
      // TODO more optimizations
      next :: acc match {
        case Move(x1)     :: Move(x2)         :: rest => Move(x1 + x2) :: rest
        case Change(x1)   :: Change(x2)       :: rest => Change(x1 + x2) :: rest
        case (loop: Loop) :: (_: Loop)        :: rest => loop :: rest
        case Set(0, 0)    :: Change(x)        :: rest => Set(x, 0) :: rest
        case Set(_, x1)   :: (s @ Set(_, x2)) :: rest if x1 == x2 => s :: rest
        case etc => etc
      }
    }
  }

  /**
    * Recursively prints the tokens to stdout,
    * expanding and indenting loops.
    * @param ts list of tokens to print
    * @param indent string to be prepended as indent
    **/
  def prettyPrint(ts: Seq[Token], indent: String = "  "): String = {

    def rec(ts: Seq[Token], indentAcc: String): String =
      ts.map {
        case Loop(Nil) => indentAcc + "Loop()"
        case Loop(lts) => "%sLoop(\n%s\n%s)"
          .format(indentAcc, rec(lts, indentAcc + indent), indentAcc)
        case t => indentAcc + t
      } mkString "\n"

    rec(ts, "")
  }

  class Memory {

    val memory: Array[Int] = new Array(30000)

    private var _pos: Int = 0

    def pos: Int = _pos

    def pos_=(p: Int): Unit = {
      _pos = p % memory.length
      if(_pos < 0) {
        _pos += memory.length
      }
    }

    def current: Int = memory(_pos)

    def current_=(x: Int): Unit = {
      memory(pos) = x
    }

    override def toString: String = s"Memory(pos=$pos, current=$current)"
  }

  sealed trait Token {

    def apply(memory: Memory): Unit
  }

  case class Read(times: Int) extends Token {

    def apply(memory: Memory): Unit = {
      for(_ <- 0 until times - 1) {
        System.in.read()
      }
      memory.current = System.in.read()
    }
  }

  case class Write(times: Int) extends Token {

    def apply(memory: Memory): Unit = {
      val ch = memory.current.toChar
      for(_ <- 1 to times) {
        print(ch)
      }
    }
  }

  case class Move(times: Int) extends Token {

    override def apply(memory: Memory): Unit =
      memory.pos += times
  }

  case class Change(times: Int) extends Token {

    override def apply(memory: Memory): Unit =
      memory.current += times
  }

  case class Loop(code: Seq[Token]) extends Token {

    override def apply(memory: Memory): Unit =
      while(memory.current != 0) {
        code.foreach(_.apply(memory))
      }

    override def toString: String = s"Loop(${code mkString ", "})"
  }

  //*** Optimization tokens ***//

  case class Set(to: Int, at: Int) extends Token {

    override def apply(memory: Memory): Unit =
      memory.memory(memory.pos + at) = to
  }
}
