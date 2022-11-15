case class Environment(enclosing: Option[Environment] = None) {
  private val values = collection.mutable.Map[String, Any]()

  def define(name: String, value: Any): Unit = {
    values(name) = value
  }

  def assign(name: Token, value: Any): Unit = {
    if (values.contains(name.lexeme)) values(name.lexeme) = value
    else if (enclosing.isDefined) enclosing.get.assign(name, value)
    else throw new RuntimeError(name, s"Undefined variable '${name.lexeme}'.")
  }

  def get(name: Token): Any = {
    if (name.lexeme == "_env") values.toString
    else if (values.contains(name.lexeme)) values(name.lexeme)
    else if (enclosing.isDefined) enclosing.get.get(name)
    else throw RuntimeError(name, s"Undefined variable '${name.lexeme}'.")
  }
}
