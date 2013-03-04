package optimizer;

import java.util.Random;

/*
 * The univariate slice sampler based 
 * on the "double" procedure, described in in Neal (2003) "Slice Sampling", 
 * The Annals of Statistics 31(3), 705-767.
 * 
 * The follow java code is rewritten from C++ version by Mark Johnson,
 * 1st Ausgust 2008
 * coded by Lan Du
 */
public abstract class SliceSampler
{
	Object params;
	double min_x;
	double max_x;
	/**
     * Abstract function to implement the log pdf.
     * 
     * @param x
     * @param params
     * @return
     */
	public abstract double logpdf(double x, Object params);
	/*
	 * Wrap Mark's slice_sampler_rfc_type
	 */
	private int nfeval = 0;
	private int max_nfeval = 200;
	private double logF(double x)
	{
		if (min_x < x && x < max_x) {
		      assert ++nfeval <= max_nfeval;
		      double fx = logpdf(x, params);
		      assert !Double.isInfinite(fx); 
		      return fx;
		  }	else {
		      return Double.NEGATIVE_INFINITY;
		  }
	}
	
	public double sliceSample1D(Object params, 
			double x, 
			Random rand, 
			double min_x,
			double max_x, 
			double w, 
			int nSamples, 
			int max_nfeval)
	{
		assert !Double.isInfinite(x);
		this.params = params;
		this.min_x = min_x;
		this.max_x = max_x;
		if(max_nfeval > 0 )
			this.max_nfeval = max_nfeval;
		/*
		 * Setup the default width
		 */
		if (w <= 0.0) {
		    if (min_x > Double.NEGATIVE_INFINITY && max_x < Double.POSITIVE_INFINITY){
		      w = (max_x - min_x)/4;
		    }else{
		      w = max(((x < 0.0) ? -x : x)/4, 0.1);
		    }
		  }
		assert !Double.isInfinite(w);
		
		double logFx = logF(x);
		for(int i = 0; i < nSamples; i++)
		{
			double logY = logFx + Math.log(rand.nextDouble()+1e-100);
			assert !Double.isInfinite(logY);
			/*
			 * Build slice interval with the "Doubling procedure" in Fig.4 
			 */
			double left = x - rand.nextDouble()*w;
			double right = left + w;
			while(logY < logF(left) || logY < logF(right))
			{
				if(rand.nextDouble() < 0.5){
					left -= right - left;
				}else{
					right += right - left;
				}
			}
			/*
			 * The "shrinkage" procedure, see Fig.5 and Fig.6
			 */
			double left1 = left;
			double right1 = right;
			while(true)
			{
				double x1 = left1 + rand.nextDouble()*(right1 - left1);
				if(logY < logF(x1)){
					double left2 = left;
					double right2 = right;
					boolean D = false, acceptable = true;
					while(right2 - left2 > 1.1*w)
					{
						double M = (right2 + left2)/2.0;
						if((x < M && x1 >= M) || (x >= M && x1 < M))
							D = true;
						if(x1 < M)
							right2 = M;
						else
							left2 = M;
						if(D && logY >= logF(left2) && logY >= logF(right2)){
							acceptable = false;
							if(x1 < x)
								left1 = x1;
							else
								right1 = x1;
							break;
						}
					}
					if(acceptable){
						x = x1;
						break;
					}
				}else{
					break;
				}
			} 
			w = (4*w + (right1 - left1))/5;  
		}
		return x;
	}
	
	private double max(double a, double b){
		if(a >= b)
			return a;
		return b;
	}
	
}