object ExprInterp {
  import scala.actors.Actor
  import Actor._

  sealed trait Operator
  case object Add extends Operator
  case object Sub extends Operator
  case object Mul extends Operator
  case object Div extends Operator

  def interpOp(op: Operator, v1: Int, v2: Int): Int =
    op match {
      case Add => v1 + v2
      case Sub => v1 - v2
      case Mul => v1 * v2
      case Div => v1 / v2
    }

  sealed trait Expr
  case class Const(value: Int) extends Expr
  case class BinOp(op: Operator, e1: Expr, e2: Expr)
       extends Expr

  val eg1 =
    BinOp(Add,
          BinOp(Sub, Const(8),
                BinOp(Mul, Const(2), Const(3))),
          BinOp(Mul,
                BinOp(Add, Const(4), Const(5)),
                BinOp(Add, Const(3),
                      BinOp(Div, Const(10), Const(2)))))

  def interp(e: Expr, k: Int => Unit): Unit =
    e match {
      case Const(value) => k(value)
      case BinOp(op, e1, e2) => {
        val join = actor{
          react{ case (1, v1:Int) =>
            react{ case (2, v2:Int) =>
              k(interpOp(op,v1,v2)) }}}
        actor{
          interp(e1, (v1:Int) => join ! (1,v1))
        }
        interp(e2, (v2:Int) => join ! (2,v2))
      }
    }

  object Z {
    import scalaz.Scalaz._
    import scalaz.concurrent.{Promise, Strategy}
    import java.util.concurrent.Executors
    implicit val pool = Executors.newFixedThreadPool(5)
    implicit val s = Strategy.Executor

    def interp(e: Expr): Promise[Int] = e match {
      case Const(value) => promise(value)
      case BinOp(op, e1, e2) =>
        val p1 = promise(interp(e1))
        val p2 = interp(e2)
        for(v1 <- p1.join; v2 <- p2)
        yield interpOp(op, v1, v2)
    }

  }
}
