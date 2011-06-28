/* Using actors to implement coroutine-like soln to same-fringe. */

object SameFringe {
  import scala.actors.Actor
  import scala.actors.Actor._

  sealed trait Tree
  case class Leaf(value: Int) extends Tree
  case class Branch(left: Tree, right: Tree)
       extends Tree

  def fringe(root: Tree): List[Int] = root match {
    case Leaf(value) => List(value)
    case Branch(left, right) =>
      fringe(left) ++ fringe(right)
  }

  val t1 =
    Branch(Leaf(1),
           Branch(Branch(Leaf(2),
                         Branch(Leaf(3),Leaf(4))),
                  Branch(Leaf(5),
                         Branch(Leaf(6), Leaf(7)))))
  val t2 =
    Branch(Branch(Leaf(1),
                  Branch(Leaf(2),Leaf(3))),
           Branch(Branch(Leaf(4),Leaf(5)),
                  Branch(Leaf(6),Leaf(7))))

  val t3 = Branch(Branch(Leaf(1),     // different value at position 4
                         Branch(Leaf(2),Leaf(3))),
                  Branch(Branch(Leaf(3),Leaf(5)),
                         Branch(Leaf(6),Leaf(7))))

  val t4 = Branch(Leaf(1),              // truncated
                  Branch(Branch(Leaf(2),
                                Branch(Leaf(3),Leaf(4))),
                         Branch(Leaf(5),
                                Leaf(6))))

  case object Done

  def sameFringe(t1: Tree, t2: Tree, k: Boolean => Unit) {

    def catch_(t: Tree): Unit = t match {
      case Leaf(value) => react {
          case v:Int =>
            if(v == value) sender ! true
            else { sender ! false; exit }
          case Done => sender ! false; exit
      }
      case Branch(left, right) =>
        catch_(left) andThen catch_(right)
    }
    val catcher = actor {
      catch_(t2) andThen react {
          case Done => sender ! true
          case _ => sender ! false
      }
    }

    def pitch(t: Tree): Unit = t match {
      case Leaf(value) =>
        catcher ! value
        react {
          case true =>
          case false => k(false); exit
        }
      case Branch(left, right) =>
        pitch(left) andThen pitch(right)
    }
    actor {
      pitch(t1) andThen {
        catcher ! Done
        react {case b: Boolean => k(b)}
      }
    }

  }

  def iter(cs: List[(Tree,Tree)]): Unit = cs match {
    case Nil => ()
    case (a,b)::abs =>
      val fa = fringe(a)
      val fb = fringe(b)
      printf("\nA = %s\nfringe(A) = %s\n", a, fa)
      printf("B = %s\nfringe(B) = %s\n", b, fb)
      printf("expecting --> %s\n", fa == fb)
      printf("sameFringe(a,b) --> ")
      sameFringe(a, b, {r => println(r); assert(r == (fa == fb)); iter(abs)})
  }

  def main(args: Array[String]) {
    iter(List((t1,t2),
              (t2,t1),
              (t1,t3),
              (t3,t1),
              (t1,t4),
              (t4,t1),
              (t4,t4),
              (t2,t3),
              (t4,t2)
            ))
  }
}
