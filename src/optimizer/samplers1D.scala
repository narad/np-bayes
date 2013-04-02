package optimizer

import scala.util.Random.nextDouble


object samplers1D {
    /**
     * 02/04/2013, bborschi
     * a 1D-adaptation of this python reimplementation: http://machinelearningmisc.blogspot.com.au/2010/01/slice-sampling-in-python.html
     * of this mathlab implementation: http://homepages.inf.ed.ac.uk/imurray2/teaching/09mlss/slice_sample.m
     */
	def slicesample(x0: Double, logpdf: Double => Double, lastllh: Double = 1.0, psigma: Double=1,step_out: Boolean = true): (Double,Double) = {
	  val sigma = if (psigma<1) 1.0 else psigma
	  var last_llh = if (lastllh==1.0) logpdf(x0) else lastllh
	  var llh0 = last_llh + math.log(nextDouble())
	  var rr = nextDouble()
	  var x_l = x0
	  x_l = x_l - rr*sigma
	  var x_r = x0
	  x_r = x_r + (1-rr)*sigma
	  if (step_out) {
	    var llh_l = logpdf(x_l)
	    while (llh_l>llh0) {
//		  System.err.println("incrleft")
	      x_l = x_l - sigma
	      llh_l = logpdf(x_l)
	    }
	    var llh_r = logpdf(x_r)
	    while (llh_r>llh0) {
//		  System.err.println("incrright")	      
	      x_r = x_r+sigma
	      llh_r = logpdf(x_r)
	    }
	  }
	  var x_cur = x0
	  var iter = true
	  while (iter) {
		  var xd = nextDouble*(x_r-x_l)+x_l
		  x_cur = xd
		  last_llh = logpdf(x_cur)
		  if (last_llh>llh0) {
		    iter=false
		  } else if (xd > x0) {
		    x_r = xd
		    System.err.println("xd>x0")
		  } else if (xd<x0) {
		    x_l = xd
		    System.err.println("xd<x0")
		  } else {
		    throw new Error("Slice sampler shrank too far")
		  }
	  }
	  (x_cur, last_llh)
	}
	
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