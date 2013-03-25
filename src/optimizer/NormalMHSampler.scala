package optimizer
import npbayes.distributions._
import scala.util.Random.nextDouble

class NormalMHSampler(logpdf: Double=>Double, min_x: Double=0.0, max_x: Double=Double.PositiveInfinity, std: Double = 0.1, isRatio: Boolean = true) {
	def sample(xp: Double,nSamples: Int,debug: Boolean=false): Double = {
	  var x = xp
	  var reject = 0
	  for (i <- 0 until nSamples) {
		  val y = truncatedNormalVariate(x, {if(isRatio) std*x else std}, min_x,max_x)
	      val lq_xy = math.log(truncatedNormal(x, y,{if (isRatio) std*y else std},0))
	      val lq_yx = math.log(truncatedNormal(y, x,{if (isRatio) std*x else std},0))
	      val lpi_y = logpdf(y)
	      val lpi_x = logpdf(x)
	      if (nextDouble<((lpi_y+lq_xy)-(lpi_x+lq_yx))) { //only change when accept 
	        x=y
	      }
	      else {
	        reject+=1
	      }
	  }
	  if (debug)
	    System.err.println("rejrate: "+reject.toFloat/nSamples+" last x:"+x)
	  x
	}
}