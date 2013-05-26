package npbayes

import scala.collection.mutable.HashMap
import scala.util.Random
import org.apache.commons.math3.special.Gamma
import scala.collection.mutable.ListBuffer


/**
 * convenience functions
 */


/**
 * Jason Naradowsky's Argument parser
 * added a toString-method
 */
class ArgParser(argArray: Array[String]) {
	val arguments = new HashMap[Any,Any]
	val params = new ListBuffer[(String,Any)]
	var args = argArray
	
	def contains(str: String): Boolean = args.contains(str)
	
	def getInt(arg: String): Int = getString(arg).toInt

	def getInt(arg: String, default: Int): Int = getString(arg, default.toString).toInt
	
	def getDouble(arg: String): Double = getString(arg).toDouble

	def getDouble(arg: String, default: Double): Double = getString(arg, default.toString).toDouble
	
	def getString(arg: String): String = getString(arg, null.asInstanceOf[String])	
		
	def getString(arg: String, default: String): String = {
	  if (args.contains(arg)) {
			return args(args.indexOf(arg)+1)
		}
		else {
			return default
		}
	}	
	
	def getBoolean(arg: String, default: Boolean=false): Boolean = {
	  if (args.contains(arg)) {
			return args(args.indexOf(arg)+1).toLowerCase == "true"
		}
		else {
			return default
		}			
	}
	
	override def toString = {
	  val res: StringBuffer = new StringBuffer
	  for ((arg,v) <- params)
	    res.append("# --"+arg+" "+v.toString()+"\n")
	  res.toString
	}
	  
	
/*	def addOption(arg: String, value: String) = {
	  args = (Array(arg, value) ++ args)
	}*/
}

class Histogram {
  val counts: HashMap[Any,Int] = new HashMap
  var total: Double = 0
  
  def incr(obs: Any) = {
    counts(obs)=counts.getOrElse(obs, 0)+1
    total += 1
  }
  
  override def toString = {
    val res = new StringBuilder
    for ((obs,count) <- counts.toList.sortBy(x=>(-x._2)))
      res.append(obs+": "+count/total+"\n")
    res.toString
  }
  
}

object Utils {
    val unif = new Random
      
    /**
     * decrements the integer-value of key by step (default=1) and
     * drops key from the map if the value drops to or below 0
     */
	def decr[T](hm: HashMap[T,Int], key: T, step: Int = 1): Unit = {
	  val old = hm(key)
	  if (old-step<=0)
	    hm.remove(key)
	  else
	    hm(key) = old-step
	}
	
	/**
	 * increments the integer-value of key by step (default=1) and
	 * ensures that a new key-value pair is added if key is not
	 * yet present
	 */
	def incr[T](hm: HashMap[T,Int], key: T, step: Int = 1): Unit = 
	  hm(key) = hm.getOrElse(key, 0)+step
	
	
	// the apache-commons implementations of some of these distributions are buggy, hence use
	// these instead (taken from Mark Johnson's Adaptor Grammar implementation)
	/**
	 * this is the characterization in terms of shape and scale
	 * p(x|a,b) = x**(shape-1) * scale**(-shape) * * exp(-x/scale) * Gamma(shape)**-1 
	 */
	def lgammadistShapeScale (x: Double, shape: Double, scale: Double): Double =
		(shape-1)*math.log(x) - shape*math.log(scale) - x/scale - org.apache.commons.math3.special.Gamma.logGamma(shape);
	
	/**
	 * this is the more Bayesian version in terms of shape and rate (beta)
	 */
	def lgammadistShapeRate (x: Double, shape: Double, rate: Double): Double =
	  shape*math.log(rate)-org.apache.commons.math3.special.Gamma.logGamma(shape)+
	  (shape-1)*math.log(x)-rate*x
	  
	def gammavariate (alpha: Double, beta: Double): Double = {
	  assert(alpha > 0);
	  assert(beta > 0);

	  if (alpha > 1.0) {
	    
	    /* Uses R.C.H. Cheng, "The generation of Gamma variables with
	       non-integral shape parameters", Applied Statistics, (1977), 26,
	       No. 1, p71-74 */

	    var ainv = math.sqrt(2.0 * alpha - 1.0);
	    var bbb = alpha - math.log(4.0);
	    var ccc = alpha + ainv;
	    var dummy = true
	    var res = 0.0
	    while (dummy) {
	      var u1 = unif.nextDouble();//mt_genrand_real3();
	      if (u1 > 1e-7  || u1 < 0.9999999) {
			  var u2 = 1.0 - unif.nextDouble();//mt_genrand_real3();
			  var v = math.log(u1/(1.0-u1))/ainv;
			  var x = alpha*math.exp(v);
			  var z = u1*u1*u2;
			  var r: Double = bbb+ccc*v-x;
			  if (r + (1.0+math.log(4.5)) - 4.5*z >= 0.0 || r >= math.log(z)) {
			    res = x*beta
			    dummy = false
			  }
	      }
	    }
	    res
	  } else if (alpha == 1.0) {
	    var u: Double = unif.nextDouble();//mt_genrand_real3();
	    while (u <= 1e-7) {
	      u = unif.nextDouble();//mt_genrand_real3();
	    }
	    -math.log(u) * beta;
	  }
	  else { 
	    /* alpha is between 0 and 1 (exclusive) 
	       Uses ALGORITHM GS of Statistical Computing - Kennedy & Gentle */
	    var dummy = true
	    var res = 0.0
	    while (dummy) {
	      var u: Double = unif.nextDouble();//mt_genrand_real3();
	      var b: Double = (math.exp(1) + alpha)/math.exp(1);
	      var p: Double = b*u;
	      var x: Double = if (p <= 1.0) math.pow(p, 1.0/alpha) else -math.log((b-p)/alpha)
	      var u1: Double = unif.nextDouble();//mt_genrand_real3();
	      if (! (((p <= 1.0) && (u1 > math.exp(-x))) ||
		     ((p > 1.0)  &&  (u1 > math.pow(x, alpha - 1.0))))) {
	        dummy=false
	    	res = x * beta;
	      }
	    }
	    res
	  }
	}
	
	def betavariate(alpha: Double, beta: Double): Double = {
		  val x = gammavariate(alpha, 1)
		  val y = gammavariate(beta, 1)
		  x/(x+y);
	}
	
	/* lbetadist() returns the log probability density of x under a Beta(alpha,beta)
	 * distribution.
	 */
	def lbetadist(x: Double, alpha: Double, beta: Double): Double = {
	  if (x<0 || x>1) {
		  Double.NegativeInfinity
	  } else
	    (alpha-1)*math.log(x)+(beta-1)*math.log(1-x)+Gamma.logGamma(alpha+beta)-Gamma.logGamma(alpha)-Gamma.logGamma(beta);
	}	
	
	
	/**
	 * from stackexchange: http://stackoverflow.com/questions/9547160/how-to-measure-time-of-a-statement-in-scala-console
	 */
	def time[A](f: => A) = {
		val s = System.nanoTime
		val ret = f
		println("time: "+(System.nanoTime-s)/1e6+"ms")
		ret
	}
}