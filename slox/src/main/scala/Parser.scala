import TokenType._

class ParseError extends LoxError("")

case class Parser(tokens: Seq[Token]) {
  private var current = 0

  def parse(): Seq[Stmt] = {
    val statements = collection.mutable.Buffer[Stmt]()
    while (!isAtEnd) statements.append(declaration())
    statements.toSeq
  }

  private def declaration(): Stmt = {
    try {
      if (_match(FUN)) functionDeclaration("function")
      else if (_match(VAR)) varDeclaration()
      else statement()
    } catch {
      case err: ParseError =>
        println(err)
        synchronize()
        null
    }
  }

  private def functionDeclaration(kind: String): Stmt.Function = {
    val name = consume(IDENTIFIER, s"Expected $kind name.")
    consume(LEFT_PAREN, s"Expected '(' after $kind name.")
    val parameters = collection.mutable.Buffer[Token]()
    if (!check(RIGHT_PAREN)) {
      do {
        if (parameters.length >= 255) error(peek, "Can't have more than 255 parameters.")
        parameters.append(consume(IDENTIFIER, "Expected parameter name."))
      } while (_match(COMMA))
    }
    consume(RIGHT_PAREN, "Expected ')' after parameters.")

    consume(LEFT_BRACE, s"Expected '{' before $kind body.")
    val body = block()
    Stmt.Function(name, parameters.toSeq, body)
  }

  private def varDeclaration(): Stmt = {
    val name = consume(IDENTIFIER, "Expected variable name.")
    val initializer = if (_match(EQUAL)) Some(expression()) else None
    consume(SEMICOLON, "Expected ';' after variable declaration.")
    Stmt.Var(name, initializer)
  }

  private def statement(): Stmt = {
    if (_match(FOR)) forStatement()
    else if (_match(IF)) ifStatement()
    else if (_match(PRINT)) printStatement()
    else if (_match(RETURN)) returnStatement()
    else if (_match(WHILE)) whileStatement()
    else if (_match(LEFT_BRACE)) Stmt.Block(block())
    else expressionStatement()
  }

  private def forStatement(): Stmt = {
    consume(LEFT_PAREN, "Expected '(' after 'for'.")

    val initializer =
      if (_match(SEMICOLON)) Stmt.Empty()
      else if (_match(VAR)) varDeclaration()
      else expressionStatement()

    val condition =
      if (check(SEMICOLON)) Expr.Literal(true)
      else expression()
    consume(SEMICOLON, "Expected ';' after loop condition.")

    val increment =
      if (check(RIGHT_PAREN)) Stmt.Empty()
      else Stmt.Expression(expression())
    consume(RIGHT_PAREN, "Expected ')' after for clause.")

    val body = statement()

    Stmt.Block(Seq(
      initializer,
      Stmt.While(condition, Stmt.Block(Seq(
        body,
        increment
      )))
    ))
  }

  private def ifStatement(): Stmt = {
    consume(LEFT_PAREN, "Expected '(' after 'if'.")
    val condition = expression()
    consume(RIGHT_PAREN, "Expected ')' after if condition.")

    val thenBranch = statement()
    val elseBranch = if (_match(ELSE)) Some(statement()) else None

    Stmt.If(condition, thenBranch, elseBranch)
  }

  private def printStatement(): Stmt = {
    val value = expression()
    consume(SEMICOLON, "Expected ';' after value.")
    Stmt.Print(value)
  }

  private def returnStatement(): Stmt = {
    val keyword = previous
    val value =
      if (check(SEMICOLON)) None
      else Some(expression())

    consume(SEMICOLON, s"Expected ';' after return${if (value == null) "" else " value"}.")

    Stmt.Return(keyword, value)
  }

  private def whileStatement(): Stmt = {
    consume(LEFT_PAREN, "Expected '(' after 'while'.")
    val condition = expression()
    consume(RIGHT_PAREN, "Expected ')' after condition.")
    val body = statement()

    Stmt.While(condition, body)
  }

  private def expressionStatement(): Stmt = {
    val expr = expression()
    consume(SEMICOLON, "Expected ';' after expression.")
    Stmt.Expression(expr)
  }

  private def block(): Seq[Stmt] = {
    val statements = collection.mutable.Buffer[Stmt]()

    while (!check(RIGHT_BRACE) && !isAtEnd) {
      statements.append(declaration());
    }

    consume(RIGHT_BRACE, "Expected '}' after block.")
    statements.toSeq
  }

  private def expression(): Expr = assignment()

  private def assignment(): Expr = {
    val expr = or()

    if (_match(EQUAL)) {
      val equals = previous
      val value = assignment()

      expr match {
        case Expr.Variable(name) => return Expr.Assign(name, value)
        case _ => error(equals, "Invalid assignment target.")
      }
    }
    expr
  }

  private def or(): Expr = logicalExpression(OR, and)
  private def and(): Expr = logicalExpression(AND, equality)
  private def equality(): Expr = binaryExpression(Seq(BANG_EQUAL, EQUAL_EQUAL), comparison)
  private def comparison(): Expr = binaryExpression(Seq(GREATER, GREATER_EQUAL, LESS, LESS_EQUAL), term)
  private def term(): Expr = binaryExpression(Seq(MINUS, PLUS), factor)
  private def factor(): Expr = binaryExpression(Seq(SLASH, STAR, PERCENT), unary)

  private def unary(): Expr = {
    if (_match(BANG, MINUS)) {
      val operator = previous
      val right = unary()
      Expr.Unary(operator, right)
    } else call()
  }

  private def call(): Expr = {
    var expr = primary()
    var matching = true

    while (matching) {
      if (_match(LEFT_PAREN)) expr = finishCall(expr)
      else matching = false
    }

    expr
  }

  private def finishCall(callee: Expr): Expr = {
    val arguments = collection.mutable.Buffer[Expr]()
    if (!check(RIGHT_PAREN)) {
      do {
        if (arguments.size >= 255) error(peek, "Can't have more than 255 arguments.")
        arguments.append(expression())
      } while (_match(COMMA))
    }

    val paren = consume(RIGHT_PAREN, "Expected ')' after arguments.")
    Expr.Call(callee, paren, arguments.toSeq)
  }

  private def primary(): Expr = {
    if (_match(FALSE)) Expr.Literal(false)
    else if (_match(TRUE)) Expr.Literal(true)
    else if (_match(NIL)) Expr.Literal(null)
    else if (_match(NUMBER, STRING)) Expr.Literal(previous.literal)
    else if (_match(IDENTIFIER)) Expr.Variable(previous)
    else if (_match(LEFT_PAREN)) {
      val expr = expression()
      consume(RIGHT_PAREN, "Expected ')' after expression.")
      Expr.Grouping(expr)
    } else throw error(peek, "Expected expression.")
  }

  private def binaryExpression(types: Seq[TokenType], next: () => Expr): Expr = {
    var expr = next()

    while (_match(types: _*)) {
      val operator = previous
      val right = next()
      expr = Expr.Binary(expr, operator, right)
    }

    expr
  }

  private def logicalExpression(_type: TokenType, next: () => Expr): Expr = {
    var expr = next()

    while (_match(_type)) {
      val operator = previous
      val right = next()
      expr = Expr.Logical(expr, operator, right)
    }

    expr
  }

  private def _match(types: TokenType*): Boolean = {
    if (types.exists(check)) {
      advance()
      true
    } else false
  }

  private def consume(_type: TokenType, message: String): Token = {
    if (check(_type)) advance()
    else throw error(peek, message)
  }

  private def check(_type: TokenType): Boolean = {
    if (isAtEnd) false else peek._type == _type
  }

  private def advance(): Token = {
    if (!isAtEnd) current += 1
    previous
  }

  private def isAtEnd: Boolean = peek._type == EOF
  private def peek: Token = tokens(current)
  private def previous: Token = tokens(current - 1)

  private def error(token: Token, message: String): ParseError = {
    Lox.error(token, message)
    new ParseError()
  }

  private def synchronize(): Unit = {
    advance()

    while (!isAtEnd) {
      if (previous._type == SEMICOLON) return

      peek._type match {
        case CLASS | FUN | VAR | FOR | IF | WHILE | PRINT | RETURN => return
      }

      advance()
    }
  }
}
