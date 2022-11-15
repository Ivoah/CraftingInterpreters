class Expr
object Expr {
  case class Binary(left: Expr, operator: Token, right: Expr) extends Expr
  case class Grouping(expression: Expr) extends Expr
  case class Literal(value: Any) extends Expr
  case class Unary(operator: Token, right: Expr) extends Expr
  case class Variable(name: Token) extends Expr
  case class Assign(name: Token, value: Expr) extends Expr
  case class Logical(left: Expr, operator: Token, right: Expr) extends Expr
  case class Call(callee: Expr, paren: Token, arguments: Seq[Expr]) extends Expr
}
