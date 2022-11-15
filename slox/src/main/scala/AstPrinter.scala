object AstPrinter {
  def print(stmt: Stmt): String = {
    "(stmt)"
  }

   def print(expr: Expr): String = {
    expr match {
      case bin: Expr.Binary => parenthesize(bin.operator.lexeme, bin.left, bin.right)
      case group: Expr.Grouping => parenthesize("group", group.expression)
      case literal: Expr.Literal => if (literal.value == null) "nil" else literal.value.toString
      case unary: Expr.Unary => parenthesize(unary.operator.lexeme, unary.right)
    }
  }

  private def parenthesize(name: String, exprs: Expr*): String = {
    s"($name ${exprs.map(print).mkString(" ")})"
  }
}
