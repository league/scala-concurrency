object ApsCounter {
  import scala.actors.Actor
  import Actor._
  case object Incr
  case class Get(k: Actor)
  val counter = actor {
    var n = 0
    loop {
      react {
        case Incr => n += 1
        case Get(k) => k ! n
      }
    }
  }


}
