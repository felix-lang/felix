import java.lang.System

object Test {
  val max_iterations = 99888

  def iterate(ci:Double, cr:Double) = {
    val bailout = 4.0
    def loop(zi:Double, zr:Double, i:Int):Int =
      if (i <= max_iterations) {
        val zr2 = zr * zr
        val zi2 = zi * zi
        if (zi2 + zr2 <= bailout) {
          loop(zr * zi * 2.0 + ci, zr2 - zi2 + cr, i + 1)
        } else {
          i
        }
      } else {
        0
      }

    loop(0.0, 0.0, 1)
  }

  def main(args:Array[String]) {
    val t0 = java.lang.System.currentTimeMillis()
    for (y <- -39 until 39) {
      println()
      for (x <- -39 until 39) {
        val i = iterate((x:Double) / 40.0, (y:Double) / 40.0 - 0.5)
        print (if (i == 0) "*" else " ")
      }
    }
    val t1 = java.lang.System.currentTimeMillis()
    println()
    println((t1 - t0) / 1000.0)
  }
}
