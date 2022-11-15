import scala.io.{Source, StdIn}

class LoxError(message: String) extends RuntimeException(message)

object Lox {
  val interpreter: Interpreter = Interpreter()
  var hadError = false
  var hadRuntimeError = false

  def main(args: Array[String]): Unit = {
    args.length match {
      case 0 =>
        runPrompt()
      case 1 =>
        runFile(args(0))
      case _ =>
        println("Usage: jlox [script]")
        System.exit(64)
    }
  }

  def runFile(path: String): Unit = {
    val source = Source.fromFile(path)
    val code = source.mkString
    source.close()
    run(code)
    if (hadError) System.exit(65)
    if (hadRuntimeError) System.exit(70)
  }

  def runPrompt(prompt: String = "> "): Unit = {
    while (true) {
      val line = StdIn.readLine(prompt)
      line match {
        case null =>
          return
        case _ =>
          try {
            run(line)
            hadError = false
            hadRuntimeError = false
          } catch {
            case err: LoxError =>
          }
      }
    }
  }

  def run(code: String): Unit = {
    val scanner = Scanner(code)
    val tokens = scanner.scanTokens()
    val parser = Parser(tokens)
    val ast = parser.parse()

    interpreter.interpret(ast)
  }

  def error(line: Int, message: String): Unit = {
    report(line, "", message)
  }

  def report(line: Int, where: String, message: String): Unit = {
    System.err.println(s"[line $line] Error$where: $message")
    hadError = true
  }

  def error(token: Token, message: String): Unit = {
    if (token._type == TokenType.EOF) {
      report(token.line, " at end", message)
    } else {
      report(token.line, s" at '${token.lexeme}'", message)
    }
  }

  def runtimeError(err: RuntimeError): Unit = {
    System.err.println(s"[line ${err.token.line}] ${err.getMessage}")
    hadRuntimeError = true
  }
}
