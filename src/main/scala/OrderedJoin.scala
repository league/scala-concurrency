object OrderedJoin {
  import scala.actors.Actor._

  val orderedJoin = actor {
    react{ case (1, x) =>
      react{ case (2, y) => println(x,y) }}}

}
