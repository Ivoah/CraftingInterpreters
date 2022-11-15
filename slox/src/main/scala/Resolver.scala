case class Resolver(interpreter: Interpreter) {
  private val scopes = collection.mutable.Stack[collection.mutable.Map[String, Boolean]]()

  private def resolve(stmt: Stmt): Unit = {
    stmt match {
      case block: Stmt.Block => resolveBlock(block)
      case _var: Stmt.Var => resolveVar(_var)
      case function: Stmt.Function => resolveFunction(function)
    }
  }

  private def resolveBlock(stmt: Stmt.Block): Unit = {
    beginScope()
    stmt.statements.foreach(resolve)
    endScope()
  }

  private def resolveVar(stmt: Stmt.Var): Unit = {
    declare(stmt.name)
    stmt.initializer.foreach(resolve)
    define(stmt.name)
  }

  private def resolveFunction(stmt: Stmt.Function): Unit = {
    declare(stmt.name)
    define(stmt.name)

    resolveCallable(stmt)
  }

  private def resolveCallable(function: Stmt.Function): Unit = {
    beginScope()
    for (p <- function.parameters) {
      declare(p)
      define(p)
    }
    resolve(function.body)
    endScope()
  }

  private def resolve(expr: Expr): Unit = {
    expr match {
      case variable: Expr.Variable => resolveVariable(variable)
      case assign: Expr.Assign => resolveAssign(assign)
    }
  }

  private def resolveVariable(expr: Expr.Variable): Unit = {
    if (scopes.headOption.exists(_(expr.name.lexeme) == false)) {
      Lox.error(expr.name, "Can't read local variable in its own initializer.")
    }

    resolveLocal(expr, expr.name)
  }

  private def resolveAssign(expr: Expr.Assign): Unit = {
    resolve(expr.value)
    resolveLocal(expr, expr.name)
  }

  private def resolveLocal(expr: Expr, name: Token): Unit = {
    for (i <- scopes.size - 1 to 0 by -1) {
      if (scopes(i).contains(name.lexeme)) {
        interpreter.resolve(expr, scopes.size - 1 - i)
        return
      }
    }
  }

  private def beginScope(): Unit = {
    scopes.push(collection.mutable.Map[String, Boolean]())
  }

  private def endScope(): Unit = {
    scopes.pop()
  }

  private def declare(name: Token): Unit = {
    scopes.headOption.foreach(_(name.lexeme) = false)
  }

  private def define(name: Token): Unit = {
    scopes.headOption.foreach(_(name.lexeme) = true)
  }
}
