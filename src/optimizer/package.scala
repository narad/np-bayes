package object optimizer {
	val eps = 1
	def approxGradientDescent1D(x: Double, f: Double => Double, threshold: Double = 0.000000, maxIters: Int = 100000, pstepSize: Double = 0.5,minX: Double =0.0): Double = {
		def approxGrad(y: Double) = {
			val y1 = f(y)
			val y2 = f(y+eps)
			(y1-y2)/(eps)
		}
		var oldL = f(x)           
		var newL = Double.NegativeInfinity
		var stepSize = pstepSize
		var iters = 0
		var deltaLL = (oldL-newL).abs
		var cur = x
		//    println("# loglikelihood at start: "+oldL)
		while (deltaLL>threshold && iters<maxIters) {
		  val g = approxGrad(x)
		  val newX = x - g*stepSize
		  if (newX<=minX)
		    stepSize = stepSize / 2.0
		  else {
			  newL = f(newX)
			  deltaLL = (newL-oldL).abs      
			  if (newL<oldL) {
			    stepSize = stepSize / 2.0
			    //println("half step-size (now "+stepSize+")")
			  } else {
			    stepSize = 1.2 * stepSize
				//println("increase step-size (now "+stepSize+")")
			    cur = newX
			    oldL = newL
			  }
		  }
		  iters+=1	  
		}
		print("After "+iters+" iterations\n")
		cur
	}
}