object ReplyCounter {
  import scala.actors.Actor._
  case object Incr
  case object Get
  val counter = actor {
    var n = 0
    loop {
      react {
        case Incr => n += 1
        case Get => sender ! n
      }
    }
  }


}
