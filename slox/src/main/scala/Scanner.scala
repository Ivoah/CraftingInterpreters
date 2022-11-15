import Scanner.keywords
import TokenType._

object Scanner {
  val keywords = Map(
    "and" -> AND,
    "class" -> CLASS,
    "else" -> ELSE,
    "false" -> FALSE,
    "for" -> FOR,
    "fun" -> FUN,
    "if" -> IF,
    "nil" -> NIL,
    "or" -> OR,
    "print" -> PRINT,
    "return" -> RETURN,
    "super" -> SUPER,
    "this" -> THIS,
    "true" -> TRUE,
    "var" -> VAR,
    "while" -> WHILE
  )
}

case class Scanner(source: String) {
  implicit class CharacterImplicits(c: Char) {
    def isIdentifierStart: Boolean = {
      c.isLetter || c == '_'
    }

    def isIdentifier: Boolean = {
      c.isIdentifierStart || c.isDigit
    }
  }

  private val tokens = collection.mutable.Buffer[Token]()
  private var start = 0
  private var current = 0
  private var line = 0

  def scanTokens(): Seq[Token] = {
    while (!isAtEnd) {
      start = current
      scanToken()
    }

    tokens.append(Token(EOF, "", null, line))
    tokens.toSeq
  }

  private def isAtEnd = current >= source.length

  private def scanToken(): Unit = {
    val c = advance()
    c match {
      case '(' => addToken(LEFT_PAREN)
      case ')' => addToken(RIGHT_PAREN)
      case '{' => addToken(LEFT_BRACE)
      case '}' => addToken(RIGHT_BRACE)
      case ',' => addToken(COMMA)
      case '.' => addToken(DOT)
      case '-' => addToken(MINUS)
      case '+' => addToken(PLUS)
      case ';' => addToken(SEMICOLON)
      case '*' => addToken(STAR)
      case '%' => addToken(PERCENT)
      case '!' => addToken(if (_match('=')) BANG_EQUAL else BANG)
      case '=' => addToken(if (_match('=')) EQUAL_EQUAL else EQUAL)
      case '<' => addToken(if (_match('=')) LESS_EQUAL else LESS)
      case '>' => addToken(if (_match('=')) GREATER_EQUAL else GREATER)

      case '/' =>
        if (_match('/')) {
          while (peek != '\n' && !isAtEnd) advance()
        } else {
          addToken(SLASH)
        }

      case ' ' | '\r' | '\t' =>
      case '\n' => line += 1

      case '"' => string()
      case _ if c.isDigit => number()
      case _ if c.isIdentifierStart => identifier()

      case _ => Lox.error(line, "Unexpected character.")
    }
  }

  private def string(): Unit = {
    while (peek != '"' && !isAtEnd) {
      if (peek == '\n') line += 1
      advance()
    }

    if (isAtEnd) {
      Lox.error(line, "Unterminated string.")
      return
    }

    advance()

    addToken(STRING, source.substring(start + 1, current - 1))
  }

  private def number(): Unit = {
    while (peek.isDigit) advance()

    if (peek == '.' && peekNext.isDigit) {
      advance()
      while (peek.isDigit) advance()
    }

    addToken(NUMBER, source.substring(start, current).toDouble)
  }

  private def identifier(): Unit = {
    while (peek.isIdentifier) advance()

    val text = source.substring(start, current)
    addToken(keywords.getOrElse(text, IDENTIFIER))
  }

  private def _match(expected: Char): Boolean = {
    if (peek == expected) {
      current += 1
      true
    } else false
  }

  private def peek: Char = {
    if (isAtEnd) '\u0000'
    else source.charAt(current)
  }

  private def peekNext: Char = {
    if (current + 1 >= source.length) '\u0000'
    else source.charAt(current + 1)
  }

  private def advance(): Char = {
    val c = source.charAt(current)
    current += 1
    c
  }

  private def addToken(_type: TokenType.Value, literal: Any = null): Unit = {
    val text = source.substring(start, current)
    tokens.append(Token(_type, text, literal, line))
  }
}
