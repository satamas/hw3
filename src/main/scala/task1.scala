import scala.util.parsing.combinator.JavaTokenParsers

/**
 * Created by atamas on 19.10.14.
 */

abstract class ASTNode

case class ASTPlus(left: ASTNode, right: ASTNode) extends ASTNode

case class ASTMinus(left: ASTNode, right: ASTNode) extends ASTNode

case class ASTMult(left: ASTNode, right: ASTNode) extends ASTNode

case class ASTDiv(left: ASTNode, right: ASTNode) extends ASTNode

case class ASTNum(value: Double) extends ASTNode


object RPNParser extends JavaTokenParsers{

  def expression: Parser[ASTNode] = (factor ~ terms | number) ^^ {
    case number: ASTNode => number
    case (expression: ASTNode) ~ (list: List[(ASTNode, (ASTNode, ASTNode) => ASTNode)]) =>
      list.foldLeft(expression)({ case (left, (right, operation)) => operation(left, right)})
  }

  def factor: Parser[ASTNode] = number ~ expression ~ operation ^^ { case num ~ expr ~ op => op(num, expr)}

  def terms: Parser[List[(ASTNode, (ASTNode, ASTNode) => ASTNode)]] = rep(expression ~ operation) ^^ {
    case terms => terms.map({ case expr ~ operation => (expr, operation)})
  }

  private def number: Parser[ASTNum] = floatingPointNumber ^^ (n => ASTNum(n.toDouble))

  private def operation = ("+" | "-" | "*" | "/") ^^{
    case "+" => ASTPlus
    case "-" => ASTMinus
    case "*" => ASTMult
    case "/" => ASTDiv
  }

  def parse(input: String) = parseAll(expression, input)
}

object Evaluator{
  def evaluate(node: ASTNode): Double = node match {
    case ASTPlus(left, right) => evaluate(left) + evaluate(right)
    case ASTMinus(left, right) => evaluate(left) - evaluate(right)
    case ASTMult(left, right) => evaluate(left) * evaluate(right)
    case ASTDiv(left, right) => evaluate(left) / evaluate(right)
    case ASTNum(num) => num
  }
}

object task1 extends App{
  println(Evaluator.evaluate(RPNParser.parse("1 2 + 4 * 3 +").get))
}
