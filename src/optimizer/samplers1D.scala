package optimizer

/**
 * univariate samplers, e.g. for hyper-parameter sampling
 */

import scala.util.Random.nextDouble


object samplers1D {

    /**
     * a 1D-adaptation of this python reimplementation: http://machinelearningmisc.blogspot.com.au/2010/01/slice-sampling-in-python.html
     * of this mathlab implementation: http://homepages.inf.ed.ac.uk/imurray2/teaching/09mlss/slice_sample.m
     */
	def slicesampleCheck(pxx: Double, llh_func: Double=>Double, plast_llh: Double = 1.0, sigma: Double=1): (Double,Double) = {
	  var xx = pxx
	  var last_llh: Double = if (plast_llh==1.0) llh_func(xx) else plast_llh
	  val llh0 = last_llh + math.log(nextDouble())
	  var rr = nextDouble()
	  var x_l = xx
	  var x_r = xx
	  x_r = x_r + (1-rr)*sigma
	  var llh_l = llh_func(x_l)
	  var llh_r = llh_func(x_r)
	  while (llh_l>llh0) {
	    x_l = x_l-sigma
	    llh_l = llh_func(x_l)
	  }
	  while (llh_r>llh0) {
	    x_r = x_r+sigma
	    llh_r = llh_func(x_r)
	  }
	  var x_cur = xx
	  var repeat = true
	  while (repeat) {
	    val xd = nextDouble()*(x_r-x_l)+x_l
	    x_cur = xd
	    last_llh = llh_func(x_cur)
	    if (last_llh>llh0) {
	      xx = xd
	      repeat = false
	    } else if (xd>xx) {
	      x_r = xd
	    } else if (xd<xx) {
	      x_l = xd
	    } else {
	      throw new Error("Slice sampler shrank too far.")
	    }
	  }
	  (xx,last_llh)
	}
	
	/*
	 * faithful implementation of Neal (2003) with additive stepout
	 */
	def slicesampleAdd(x0: Double, f: Double => Double,yp: Double = 1.0, w: Double = 1.0, m: Int=Int.MaxValue): (Double, Double) = {
	  val y = if (yp==1.0) f(x0)+math.log(nextDouble()) else yp+math.log(nextDouble())
	  def stepout: (Double,Double) = {
	    val U = nextDouble()
	    var L = x0-w*U
	    var R = L+w
	    val V = nextDouble()
	    var J = (m*V).floor.toInt
	    var K = (m-1)-J
	    var fL = f(L)
	    var fR = f(R)
	    while (J>0 && y<fL) {
	      L = L-w
	      fL = f(L)
	      J = J-1
	    }
	    while (K>0 && y<fR) {
	      R = R+w
	      fR = f(R)
	      K = K-1
	    }
	    (L,R)
	  }
	  def shrinkage(LR:(Double,Double)): (Double,Double) = {
	    var LPrime = LR._1
	    var RPrime = LR._2
	    var repeat = true
	    var x1: Double = 0.0
	    var fx1: Double = 0
	    while (repeat) {
	      val U = nextDouble()
	      x1 = LPrime + U*(RPrime-LPrime)
	      fx1 = f(x1)
	      if (y<fx1) {
	        repeat=false
	      } else {
	        if (x1<x0) {
	          LPrime=x1
	        } else {
	          RPrime =x1
	        }
	      }
	    }
	    (x1,fx1)
	  }
	shrinkage(stepout)	  
	}
	  
	/**
	 * faithful implementation of Neal (2003) with doubling
	 */
	def slicesampleDouble(x0: Double, f: Double => Double, yp: Double = 1.0, w: Double=1.0,p: Int=32): (Double,Double) = {
		val y = if (yp==1.0) f(x0)+math.log(nextDouble()) else yp+math.log(nextDouble())
	    def stepout: (Double,Double) = {
		  val u = nextDouble()
		  var L = x0 - w*u
		  var R = L + w
		  var K = p
		  while (K>0 && (y<f(L) || y<f(R))) {
		    val V = nextDouble()
		    if (V<0.5) {
		      L = L - (R-L)
		    } else {
		      R = R+(R-L)
		    }
		    K-=1
		  }
		  (L,R)
		}
		
		def Accept(x1: Double, LR: (Double, Double)): Boolean = {
		  var LHat = LR._1
		  var RHat = LR._2
		  var D = false
		  while (RHat-LHat>1.1*w) {
		    val M = (LHat+RHat)/2.0
		    if ( (x0<M && x1>=M) ||
		         (x0>=M && x1<M) ) {
		      D=true
		    }
		    if (x1<M) {
		      RHat = M
		    } else {
		      LHat = M
		    }
		    if (D && y>=f(LHat) && y>=f(RHat)) {
		      return false
		    }
		  }
		  true
		}
		
		def shrinkage(LR:(Double,Double)): (Double,Double) = {
		  var LPrime = LR._1
		  var RPrime = LR._2
		  var repeat = true
		  var x1: Double = 0.0
		  var fx1: Double = 0
		  while (repeat) {
		    val U = nextDouble()
		    x1 = LPrime + U*(RPrime-LPrime)
		    fx1 = f(x1)
		    if (y<fx1 && Accept(x1,LR)) {
		      repeat=false
		    } else {
		      if (x1<x0) {
		        LPrime=x1
		      } else {
		        RPrime =x1
		      }
		    }
		  }
		  (x1,fx1)
		}
		shrinkage(stepout)
	}
	
	/**
	 * simple metropolis-hastings sampler
	 */
	def mhsample(x0: Double,logpdf: Double=>Double,proposallogpdf: (Double,Double)=>Double,proposalsample: Double=>Double,iters: Int=1,debug: Boolean=false): Double = {
	  var xcur = x0
	  var rejected = 0
	  for (i <- 0 until iters) {
	    val xnew = proposalsample(xcur)
	    val lq_yx = proposallogpdf(xnew,xcur)
	    val lq_xy = proposallogpdf(xcur,xnew)
	    val lpi_y = logpdf(xnew)
	    val lpi_x = logpdf(xcur)
	    if (math.log(nextDouble())<((lpi_y+lq_xy)-(lpi_x+lq_yx))) { //only change when accept 
	      xcur = xnew
	    } else {
	      rejected+=1
	    }
	  }
	  if (debug) {
		System.err.println("rejrate: "+rejected.toFloat/iters)	    
	  }
	  xcur
	}
}