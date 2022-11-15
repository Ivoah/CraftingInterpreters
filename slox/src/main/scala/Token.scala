object TokenType extends Enumeration {
  type TokenType = Value

  // Single-character tokens.
  val LEFT_PAREN, RIGHT_PAREN, LEFT_BRACE, RIGHT_BRACE,
      COMMA, DOT, MINUS, PLUS, SEMICOLON, SLASH, STAR, PERCENT = Value

  // One or two character tokens.
  val BANG, BANG_EQUAL,
      EQUAL, EQUAL_EQUAL,
      GREATER, GREATER_EQUAL,
      LESS, LESS_EQUAL = Value

  // Literals.
  val IDENTIFIER, STRING, NUMBER = Value

  // Keywords.
  val AND, CLASS, ELSE, FALSE, FUN, FOR, IF, NIL, OR,
      PRINT, RETURN, SUPER, THIS, TRUE, VAR, WHILE = Value

  val EOF = Value
}
import TokenType._

case class Token(_type: TokenType, lexeme: String, literal: Any, line: Int) {
  override def toString: String = s"${_type} $lexeme $literal"
}
