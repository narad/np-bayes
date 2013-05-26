package optimizer

import scala.util.Random.nextDouble

/*
 * The univariate slice sampler based 
 * on the "double" procedure, described in in Neal (2003) "Slice Sampling", 
 * The Annals of Statistics 31(3), 705-767.
 * 
 * The follow java code is rewritten from Java version Lan Du prepared off
 * the C++ version by Mark Johnson, 1st Ausgust 2008
 */
class SliceSampler(logpdf: (Double=>Double),min_x: Double, max_x: Double)
{

	
	def logF(x: Double): Double = 
	  if (min_x < x && x < max_x) {
		      val fx = logpdf(x)
//		      println("f("+x+")="+fx)
		      assert(fx!=Double.PositiveInfinity && fx!=Double.NegativeInfinity) 
		      fx
		  }	else {
		      Double.NegativeInfinity
		  }
	
	
	def sliceSample1D(xp: Double,wp: Double, nSamples: Int) = {
		var w = wp
		var x = xp
	    assert (x!=Double.PositiveInfinity && x!=Double.NegativeInfinity)
		/*
		 * Setup the default width
		 */
		if (w <= 0.0) {
		    if (min_x > Double.NegativeInfinity && max_x < Double.PositiveInfinity) {
		      w = (max_x - min_x)/4;
		    }else{
		      //w = (if (x < 0.0) -x else x)/4, 0.1))
		      w = 0.1
		    }
		  }
		assert(w!=Double.PositiveInfinity && w!=Double.NegativeInfinity)
		
		var logFx: Double = logF(x)
		for(i <- 0 to nSamples)
		{
			val logY = logFx + math.log(nextDouble()+1e-100);
			assert(logY!=Double.NegativeInfinity && logY!=Double.PositiveInfinity)
			/*
			 * Build slice interval with the "Doubling procedure" in Fig.4 
			 */
			var left = x - nextDouble()*w
			var right = left + w
			while(logY < logF(left) || logY < logF(right))
			{
				if(nextDouble() < 0.5){
					left -= right - left;
				}else{
					right += right - left;
				}
			}
			/*
			 * The "shrinkage" procedure, see Fig.5 and Fig.6
			 */
			var left1 = if (left<min_x) min_x else left
			var right1 = if (right>max_x) max_x else right
			var break=false
			while(!break)
			{
				val x1 = left1 + nextDouble()*(right1 - left1);
				break=false
				if(logY < logF(x1)){
					var left2 = left
					var right2 = right
					var D = false
					var acceptable = true
					while(right2 - left2 > 1.1*w && !break)
					{
						var M = (right2 + left2)/2.0
						if((x < M && x1 >= M) || (x >= M && x1 < M))
							D = true
						if(x1 < M)
							right2 = M
						else
							left2 = M
						if(D && logY >= logF(left2) && logY >= logF(right2)){
							acceptable = false
							if(x1 < x)
								left1 = x1
							else
								right1 = x1
							break=true
						}
					}
					break=false
					if(acceptable){
						x = x1
						break=true
					}
				}else{
					break=true
				}
			} 
			w = (4*w + (right1 - left1))/5;  
		}
		x
	}
	
	def max(a: Double, b: Double): Double = {
		if(a >= b)
		  a
		else
		  b
	}
	
}