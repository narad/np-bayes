/**
 * npbayes.distributions
 * 
 * provides both sampling and evaluation functionalities for some standard
 * distributions, such as Gaussians, Gammas, Betas, Dirichlets.
 * 
 * ==Overview==
 * The entire package consists of the distributions object which provides (static) functions. For each distribution,
 * there is usually a pair of functions, one to evaluate a pdf at a specific point and one to sample from the pdf.
 */

package npbayes
import org.apache.commons.math3.distribution.NormalDistribution
import scala.util.Random.nextDouble


package object distributions {

   /**
    * taken from factorie: https://code.google.com/p/factorie/source/browse/src/main/scala/cc/factorie/maths/Random.scala 
    */
   /** Return a random double drawn from a Gaussian distribution with mean 0 and variance 1. */
	/**
	 * @return
	 */
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
	
  
  
    /**
     * inefficient rejection sampling...
     */
	def truncatedNormalVariate(mu: Double, sigma: Double, a: Double=0, b: Double = Double.PositiveInfinity): Double = {
	  var res = nextGaussian(mu,sigma)
	  while (res<a || res>b)
	    res = nextGaussian(mu,sigma)
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