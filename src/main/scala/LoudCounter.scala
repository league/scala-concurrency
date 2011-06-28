object LoudCounter {
  import scala.actors.Actor._
  case object Incr
  val counter = actor {
    var n = 0
    loop {     // repeatedly wait for a message
      react {  // (but don't block thread)
        case Incr => n += 1; println(n)
      }
    }
  }

  counter ! Incr // fire and forget; eventually
  counter ! Incr // prints '1' then '2'

  def main(args: Array[String]) { }
}
