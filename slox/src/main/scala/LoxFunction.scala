trait LoxCallable {
  val arity: Int
  def call(interpreter: Interpreter, arguments: Seq[Any]): Any
}

case class LoxFunction(declaration: Stmt.Function, closure: Environment) extends LoxCallable {
  override val arity: Int = declaration.parameters.length

  override def call(interpreter: Interpreter, arguments: Seq[Any]): Any = {
    val environment = Environment(Some(closure))

    declaration.parameters.zip(arguments).foreach {
      case (parameter, argument) => environment.define(parameter.lexeme, argument)
    }

    try {
      interpreter.executeBlock(declaration.body, environment)
      null
    } catch {
      case Return(value) => value
    }
  }

  override def toString: String = s"<fn '${declaration.name.lexeme}'>"
}
