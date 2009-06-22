// based off http://metamatix.org/~ocaml/price-of-abstraction.html

object Test {
  val RESOLUTION = 5000

  final case class Complex(x:Double,y:Double) {
    def norm_square = x*x + y*y

    def +(other:Complex) = new Complex(x+other.x,y+other.y)

    def *(other:Complex) = new Complex(x*other.x-y*other.y,
                                       x*other.y+y*other.x)
  }


  def iters(max_iter:Int,xc:Double,yc:Double):Int = {
    val c = new Complex(xc,yc)
    def aux(count:Int,z:Complex):Int = {
      if(count >= max_iter) { max_iter } else {
        if( z.norm_square >= 4.0 ) count else {
          aux(count+1,z*z+c)
        }
      }
    }
    aux(0,c)
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
