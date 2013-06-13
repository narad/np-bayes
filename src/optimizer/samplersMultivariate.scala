package optimizer

import scala.util.Random.nextDouble

object samplersMultivariate {
	/**
	 * simple metropolis-hastings sampler
	 * 
	 */
	def mhsample(x0: Array[Double],logpdf: Array[Double]=>Double,proposallogpdf: (Array[Double],Array[Double])=>Double,proposalsample: Array[Double]=>Array[Double],burnin: Int=100,samples: Int=10,lag: Int=10,debug: Boolean=false): Array[Array[Double]] = {
	  val result = new Array[Array[Double]](samples)
	  var iteration = 0
	  var nSample = 0
	  var xcur = x0
	  var rejected = 0
	  while (iteration <= burnin+(samples-1)*lag) {
	    val xnew = proposalsample(xcur)
	    val lq_yx = proposallogpdf(xnew,xcur)
	    val lq_xy = proposallogpdf(xcur,xnew)
	    val lpi_y = logpdf(xnew)
	    val lpi_x = logpdf(xcur)
//	    print("lpi_old "+lpi_x+" lpi_new "+lpi_y+" lq_old "+lq_xy+" lq_new "+lq_yx+ " p(accept)="+math.exp((lpi_y+lq_xy)-(lpi_x+lq_yx))+"\n")
	    if (math.log(nextDouble())<((lpi_y+lq_xy)-(lpi_x+lq_yx))) { //only change when accept 
	      xcur = xnew
	    } else {
	      rejected+=1
	    }
	    if (iteration>=burnin && (iteration-burnin) % lag == 0) {
	      result(nSample)=xcur
	      nSample+=1
	    }
	    iteration+=1	    
	  }
	  if (debug) {
		System.err.println("rejrate: "+rejected.toFloat/iteration)	    
	  }
	  result
	}
}