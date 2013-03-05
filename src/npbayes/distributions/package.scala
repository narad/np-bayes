package npbayes
import org.apache.commons.math3.distribution.NormalDistribution
import scala.util.Random.nextDouble

package object distributions {
  
    /**
     * inefficient rejection sampling...
     */
	def truncatedNormalVariate(mu: Double, sigma: Double, a: Double=0, b: Double = Double.PositiveInfinity): Double = {
	  val normal = new NormalDistribution(mu, sigma)
	  var res = normal.sample()
	  while (res<a || res>b)
	    res = normal.sample
	  res
	}
	
	def truncatedNormal(x: Double, mu: Double, sigma: Double, a: Double=0, b: Double = Double.PositiveInfinity): Double = {
	  if (x<a || x>b)
	    0
	  else {
	    val normal = new NormalDistribution()
	    val FB = if (b!=Double.PositiveInfinity)
	      normal.cumulativeProbability((b-mu)/sigma)
	    else
	      1.0
	    val FA = if (a!=Double.NegativeInfinity)
	      normal.cumulativeProbability((a-mu)/sigma)
	    else
	      0.0
	    ( (1/sigma)*normal.density((x-mu)/sigma) ) /
	     (FB-FA)
	  }
	}
} 