// based off http://metamatix.org/~ocaml/price-of-abstraction.html

object Test {
  val RESOLUTION = 5000

  def iters(max_iter:Int,xc:Double,yc:Double):Int = {
    def aux(count:Int,x:Double,y:Double):Int = {
      if(count >= max_iter) { max_iter } else {
        if( x * x + y * y >= 4.0 ) count else {
          aux(count+1,x * x - y * y+xc,2.0*x*y+yc)
        }
      }
    }
    aux(0,xc,yc)
  }

  def main(args:Array[String]) {
    val t0 = java.lang.System.currentTimeMillis()
    val max_val = RESOLUTION/2
    val min_val = -RESOLUTION/2
    val mul = 2.0 / max_val
    var count = 0
    var count_tiles = 0
    for( i <- min_val to max_val; j <- min_val to max_val) {
      count += iters(100,mul*i,mul*j)
    }
    val t1 = java.lang.System.currentTimeMillis()
    println(count)
    println((t1 - t0) / 1000.0)
  }
}
