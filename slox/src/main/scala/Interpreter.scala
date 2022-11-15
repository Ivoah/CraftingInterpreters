import TokenType._

case class RuntimeError(token: Token, message: String) extends LoxError(message)
case class Return(value: Any) extends RuntimeException(null, null, false, false)

case class Interpreter() {
  val globals: Environment = Environment()
  private var environment = globals

  globals.define("clock", new LoxCallable {
    override val arity: Int = 0
    override def call(interpreter: Interpreter, arguments: Seq[Any]): Any = System.currentTimeMillis()/1000.0
    override def toString: String = "<native fn 'clock'>"
  })

  def interpret(statements: Seq[Stmt]): Unit = {
    try {
      statements.foreach(execute)
    } catch {
      case err: RuntimeError => Lox.runtimeError(err)
    }
  }

  private def stringify(value: Any): String = {
    value match {
      case null => "nil"
      case number: Double =>
        val text = number.toString
        if (text.endsWith(".0")) text.substring(0, text.length - 2)
        else text
      case _ => value.toString
    }
  }

  private def execute(stmt: Stmt): Unit = {
    stmt match {
      case _: Stmt.Empty =>
      case function: Stmt.Function => executeFunction(function)
      case print: Stmt.Print => executePrint(print)
      case expression: Stmt.Expression => executeExpression(expression)
      case variable: Stmt.Var => executeVar(variable)
      case Stmt.Block(statements) => executeBlock(statements, Environment(Some(environment)))
      case _if: Stmt.If => executeIf(_if)
      case _return: Stmt.Return => executeReturn(_return)
      case _while: Stmt.While => executeWhile(_while)
    }
  }

  private def executeFunction(stmt: Stmt.Function): Unit = {
    val function = LoxFunction(stmt, environment)
    environment.define(stmt.name.lexeme, function)
  }

  private def executePrint(stmt: Stmt.Print): Unit = {
    val value = evaluate(stmt.expression)
    println(stringify(value))
  }

  private def executeExpression(stmt: Stmt.Expression): Unit = {
    evaluate(stmt.expression)
  }

  private def executeVar(stmt: Stmt.Var): Unit = {
    val value = stmt.initializer match {
      case Some(expr) => evaluate(expr)
      case None => null
    }

    environment.define(stmt.name.lexeme, value)
  }

  def executeBlock(statements: Seq[Stmt], newEnv: Environment): Unit = {
    val prevEnv = environment
    environment = newEnv
    try {
      statements.foreach(execute)
    } finally {
      environment = prevEnv
    }
  }

  private def executeIf(stmt: Stmt.If): Unit = {
    if (isTruthy(evaluate(stmt.condition))) execute(stmt.thenBranch)
    else stmt.elseBranch.foreach(execute)
  }

  private def executeReturn(stmt: Stmt.Return): Unit = {
    val value = stmt.value.map(evaluate).orNull
    throw Return(value)
  }

  private def executeWhile(stmt: Stmt.While): Unit = {
    while (isTruthy(evaluate(stmt.condition))) execute(stmt.body)
  }

  private def evaluate(expr: Expr): Any = {
    expr match {
      case literal: Expr.Literal => evaluateLiteral(literal)
      case grouping: Expr.Grouping => evaluateGrouping(grouping)
      case unary: Expr.Unary => evaluateUnary(unary)
      case binary: Expr.Binary => evaluateBinary(binary)
      case variable: Expr.Variable => evaluateVariable(variable)
      case assign: Expr.Assign => evaluateAssign(assign)
      case logical: Expr.Logical => evaluateLogical(logical)
      case call: Expr.Call => evaluateCall(call)
    }
  }

  private def evaluateLiteral(expr: Expr.Literal): Any = {
    expr.value
  }

  private def evaluateGrouping(expr: Expr.Grouping): Any = {
    evaluate(expr.expression)
  }

  private def evaluateUnary(expr: Expr.Unary): Any = {
    val right = evaluate(expr.right)
    (expr.operator._type, right) match {
      case (BANG, r) => !isTruthy(r)
      case (MINUS, r: Double) => -r
      case _ => throw RuntimeError(expr.operator, s"Unsupported operand for '${expr.operator.lexeme}'")
    }
  }

  private def evaluateVariable(expr: Expr.Variable): Any = {
    environment.get(expr.name)
  }

  private def evaluateAssign(expr: Expr.Assign): Any = {
    val value = evaluate(expr.value)
    environment.assign(expr.name, value)
    value
  }

  private def evaluateBinary(expr: Expr.Binary): Any = {
    val left = evaluate(expr.left)
    val right = evaluate(expr.right)

    (left, expr.operator._type, right) match {
      case (l: Double, MINUS, r: Double) => l - r
      case (l: Double, SLASH, r: Double) => l / r
      case (l: Double, STAR, r: Double) => l * r
      case (l: Double, PERCENT, r: Double) => l % r
      case (l: Double, PLUS, r: Double) => l + r
      case (l: String, PLUS, r: String) => l + r
      case (l: Double, GREATER, r: Double) => l > r
      case (l: Double, GREATER_EQUAL, r: Double) => l >= r
      case (l: Double, LESS, r: Double) => l < r
      case (l: Double, LESS_EQUAL, r: Double) => l <= r
      case (l, BANG_EQUAL, r) => !isEqual(l, r)
      case (l, EQUAL_EQUAL, r) => isEqual(l, r)
      case _ => throw RuntimeError(expr.operator, s"Unsupported operands for '${expr.operator.lexeme}'")
    }
  }

  private def evaluateLogical(expr: Expr.Logical): Any = {
    val left = evaluate(expr.left)

    expr.operator._type match {
      case OR if isTruthy(left) => left
      case AND if !isTruthy(left) => left
      case _ => evaluate(expr.right)
    }
  }

  private def evaluateCall(expr: Expr.Call): Any = {
    val callee = evaluate(expr.callee)
    callee match {
      case fn: LoxCallable =>
        if (fn.arity != expr.arguments.length) throw RuntimeError(expr.paren, s"Expected ${fn.arity} arguments but got ${expr.arguments.length}.")
        fn.call(this, expr.arguments.map(evaluate))
      case _ => throw RuntimeError(expr.paren, "Can only call functions and classes.")
    }
  }

  private def isTruthy(value: Any): Boolean = {
    value match {
      case null => false
      case bool: Boolean => bool
      case _ => true
    }
  }

  private def isEqual(left: Any, right: Any): Boolean = {
    (left, right) match {
      case (null, null) => true
      case (null, _) => false
      case _ => left.equals(right)
    }
  }
}
