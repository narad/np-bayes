package npbayes
import org.apache.commons.math3.distribution.NormalDistribution
import scala.util.Random.nextDouble

package object distributions {

   /**
    * taken from factorie: https://code.google.com/p/factorie/source/browse/src/main/scala/cc/factorie/maths/Random.scala 
    */
   /** Return a random double drawn from a Gaussian distribution with mean 0 and variance 1. */
	def nextGaussian() : Double = {
	      val v1 = nextDouble()
	      val v2 = nextDouble()
	      val x1 = math.sqrt(-2*math.log(v1))*math.cos(2*math.Pi*v2)
	      val x2 = math.sqrt(-2*math.log(v1))*math.sin(2*math.Pi*v2)
	      x2
	}
	/**
    * taken from factorie: https://code.google.com/p/factorie/source/browse/src/main/scala/cc/factorie/maths/Random.scala 
    */
	/** Return a random double drawn from a Gaussian distribution with mean m and sd. */
	def nextGaussian(mean:Double, sd:Double) :Double = nextGaussian() * sd+mean  
    
	
	def gaussian(mean: Double, sd: Double, x: Double) = 
	  (1/math.sqrt(2*math.Pi*sd*sd)) * math.exp(-math.pow(x-mean,2)/(2*sd*sd))
	
	def logGaussian(mean: Double, s2: Double, x: Double) =
	  -math.log(math.sqrt(2*math.Pi*s2)) -math.pow(x-mean,2)/(2*s2)
	
    def normalVariate(mu:Double, sigma: Double): Double = 
      new org.apache.commons.math3.distribution.NormalDistribution(mu,sigma).sample()
    
    def normal(x: Double,mu: Double, sigma: Double): Double =
      new org.apache.commons.math3.distribution.NormalDistribution(mu,sigma).density(x)
  
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