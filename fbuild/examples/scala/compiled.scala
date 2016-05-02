import world.World

object HelloWorld {
  def main(args: Array[String]) {
    val w = world.World
    w.greet("compiled")
  }
}
