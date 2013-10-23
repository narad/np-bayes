package npbayes.maxent

import breeze.linalg
import breeze.optimize.DiffFunction
import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix
import scala.util.Random.nextDouble
import breeze.linalg.Matrix
import scala.util.Random

trait FeatureVector[T] {
  def features(x: T)
}


object LogisticRegression {
    
  /**
   * Turn a Matrix into an Array of the form that apache uses
   */
    def toArray(in: DenseMatrix[Double]): Array[Array[Double]] = {
      val res = Array.ofDim[Double](in.rows, in.cols)
      var i = 0
      while (i<in.rows) {
        var j = 0
	      while (j<in.cols) {
	        res(i)(j) = in(i,j)
	        j+=1
      	  }
      	  i+=1
    	}
      res
    }  
    
    /**
     * turn an Array of Arrays into a Breeze Matrix
     */
    def turnIntoMatrix(in: Array[Array[Double]]): breeze.linalg.DenseMatrix[Double] = {
      val rows: Int = in.length
      def cols: Int = in(0).length
      val tmpArray = Array.fill(rows * cols)(0.0)
      var i = 0
      for (c <- 0 until cols) {
        for (r <- 0 until rows) {
          tmpArray(i) = in(r)(c)
          i+=1
        }
      }
      breeze.linalg.DenseMatrix.create(rows, cols, tmpArray) 
    }
    
    
    /**
     * a Gaussian prior with 0 mean, this is the loglikelihood
     */
    def l2prior(strength: Double)(w: DenseVector[Double]): Double = {
      //val norm = -math.log((2*math.Pi*math.pow(variance,w.size)))
      //norm - (w map (math.pow(_,2)/(2*variance))).sum
      - strength*(w map (math.pow(_,2))).sum
    }
    
    /**
     * this is the derivative of the log-likelihood of the Gaussian prior
     */
    def l2derivative(strength:Double)(w: DenseVector[Double]): DenseVector[Double] = {
      - w*strength 
    }
    
    /**
     * a laplace prior with 0 mean, this is the loglikelihood 
     * we put independent laplace-priors on each individual weight!!
     */
    def l1prior(w: DenseVector[Double],scale: Double=1.0): Double = {
      val norm = -math.log(2*scale)*w.size
      norm - (w map (_.abs/scale)).sum
    }
    
    /**
     * subgradient for w(i)==0 --> random number between [-1;1]
     */
    def l1derivative(w: DenseVector[Double]): DenseVector[Double] = {
      val res = breeze.linalg.DenseVector.fill(w.size)(0.0)
      var i = 0
      while (i<w.size) {
        if (w(i)>0)
          res(i) = -math.sqrt(2.0)
        else if (w(i)<0)
          res(i) = math.sqrt(2.0)
        else if (w(i)==0) {
          val pos = (nextDouble()>0.5)
          if (pos)
            nextDouble()
          else
            -nextDouble()
        }
        i+=1
      }
      res
    }
}

/**
 * binary logistic regression
 */
class LogisticRegression[Input](nfeatures: Int,val features: Input => Array[Double],val logprior: (DenseVector[Double])=>Double=LogisticRegression.l2prior(1), val priorderiv: (DenseVector[Double])=>DenseVector[Double]=LogisticRegression.l2derivative(1))  {
  
  def sigmoid(x: Double) = 1/(1+math.exp(-x))
  def sigmoidprime(x: Double) = sigmoid(x)*(1-sigmoid(x))
  
  var weights: DenseVector[Double] = DenseVector.fill(nfeatures)(0.0)
  var inputs: DenseMatrix[Double] = null	// N x |w| data matrix
  var inputstranspose: DenseMatrix[Double] = null //|w| x N data matrix
  var outputs: DenseVector[Double] = null // Nx1 output vector
  
  
  var hasData: Boolean = false
  /*
   * featurize the inputs
   */
  def setInputs(in: Array[Input]) = {
    hasData = true
    inputs = LogisticRegression.turnIntoMatrix(in map (features(_)))
    inputstranspose=inputs.t
  }
  
   
  def setOutputs(out: Array[Double]) = {
    outputs = DenseVector(out)
  }
  
  /**
   * calculates the probability of the input vector coming out as positive
   */
  def prob(in: Array[Double],w: DenseVector[Double] = weights) = {
    sigmoid(w dot new DenseVector[Double](in))
  }
 
  def clipSum(x1: DenseVector[Double],x2: DenseVector[Double]): DenseVector[Double] = {
    assert(x1.size==x2.size)
    val res = new Array[Double](x1.size)
    var i = 0
    while (i<x1.size) {
      res(i) = {if (x1(i)<0) math.min(0,x1(i)+x2(i)) else math.max(0,x1(i)+x2(i))} 
      i+=1
    }
    new DenseVector[Double](res)
  }
  
  /**
   * with clipping, as described in Bob Carpenter's notes
   */
  def stochasticDescent(iters: Int=50,threshold: Double = 1e-4) = {
    var j = 1

    val n: Double = outputs.size
    var oldLL = loglikelihood()
    println("Logprob at start: "+oldLL)
    var notConverged = true
    while (j<=iters && notConverged) {
        val stepSize = 1.0/(j+1)
        val order = Random.shuffle(0 to outputs.size-1)
        var i = 0        
	    while (i < n) {
	      val xi = inputstranspose(::,order(i))
		  val mu = sigmoid(weights dot xi)
		  val unregG = (xi*(mu-outputs(order(i))))
		  //debug.println(xi.data+" "+mu+" " +outputs(order(i))+ " " +g)
		//  println("mu: "+mu+" g_"+i+": "+g)
		  weights = weights - unregG*stepSize
		  weights = weights + (priorderiv(weights)/n)*stepSize //  clipSum(weights,(priorderiv(weights)/n)*stepSize)
		  //debug.println(weights)
		  i+=1
	    }
        val newLL = loglikelihood()
        if ((newLL-oldLL).abs < threshold)
        	notConverged = false
        oldLL=newLL
        j+=1
    }
    println("Logprob at end: "+oldLL)
    //debug.close()
  } 
  
  /**
   * perform MAP-inference using LBFGS
   */
  def mapLBFGS(start: DenseVector[Double]=weights,maxIters:Int=1000,m: Int=8) = {
    println("Logprob at start: "+loglikelihood())
    val lbfgs = new breeze.optimize.LBFGS[DenseVector[Double]](maxIters,m)
    weights = lbfgs.minimize(gradient, start)
    println("Logprob at end: "+loglikelihood())
  }
  
  def mhUpdate(burnIn:Int=100) = {
	  mapLBFGS()//GradientDescent(m
//    mapGradientDescent()

/** debugging
    val posterior = new java.io.PrintStream(new java.io.File("logl.test"),"utf-8")
    var w: Double = -5.1
    while (w <= -4.9) {
      val t = DenseVector.fill(1)(w)
      posterior.println(w+" "+loglikelihood(t)+ " " + gradient.gradientAt(t).data(0))
      w += 0.001
    }
*/
//    mapNewtonsMethod()
    val g = gradient.gradientAt(weights)
    val H = hessianAt(weights)
    val H_inv = breeze.linalg.inv(hessianAt(weights))
    val gaussian =
      new org.apache.commons.math3.distribution.MultivariateNormalDistribution(weights.data,LogisticRegression.toArray(H_inv*(math.pow(2.38, 2)/weights.data.length)))
    def prop(x: Array[Double]): Array[Double] = gaussian.sample()
    def proplpdf(x: Array[Double],y: Array[Double]) = {
    	val res = math.log(gaussian.density(x))
    	res
    }

    def logpdf(x: Array[Double]) = loglikelihood(new DenseVector(x))
    weights = new DenseVector(optimizer.samplersMultivariate.mhsample(weights.data, logpdf, proplpdf, prop, burnIn, 1,1,true)(0))
  }
  
  /**
   * see HelmsHold2006, A2
   */
  def helmsHoldGibbs(start: DenseVector[Double]=weights,nu: DenseMatrix[Double],nIters: Int) = {
    val n = inputs.rows
	val Lambda = breeze.linalg.diag(breeze.linalg.DenseVector.fill(n)(1.0))
	val Z = breeze.linalg.DenseVector.fill(n)(0.0)
	def initZ = {
      var i=0
      while (i<n) {
        val (a,b) = if (outputs(i)==1) (0.0,Double.PositiveInfinity) else (Double.NegativeInfinity,0.0)
        Z(i) = npbayes.distributions.truncatedNormalVariate(0, 1, a, b)
      }
    }
    initZ
    var iteration = 0
    while (iteration<nIters) {
      val V = inputstranspose*Lambda.t
      iteration+=1
    }
  }
  
  /**
   * perform MAP-inference using Newton's method
   */
  def mapNewtonsMethod(start: DenseVector[Double]=weights,threshold: Double = 0.0001, maxIters: Int=100, pstepSize: Double=1.0) = {
    weights = start
    var oldL = loglikelihood()                   
    var newL = Double.NegativeInfinity
    var iters = 0
    var stepSize = pstepSize
    var deltaLL = (oldL-newL).abs
    var g = gradient.gradientAt(weights)    
    while (deltaLL>threshold && iters<maxIters) {
      val H = hessianAt(weights)
      val H_inv = breeze.linalg.inv(hessianAt(weights))
      val d = H_inv*g
      val newW = weights - d *stepSize
      newL = loglikelihood(newW)
      deltaLL = (newL-oldL).abs
      if (newL<oldL) {
        stepSize = stepSize / 2.0
      } else {
        stepSize = 1.2 * stepSize
        weights = newW        
        oldL = newL
        g = gradient.gradientAt(weights)
        //g = g / g.norm(2.0)
      }
      iters+=1      
    }
  }
  
  /**
   * perform MAP inference using Gradient descent
   */
  def mapGradientDescent(start: DenseVector[Double]=weights, threshold: Double = 0.0001, maxIters: Int=100, pstepSize: Double=0.5) = {
    weights = start
    var oldL = loglikelihood()           
    var newL = Double.NegativeInfinity
    var stepSize = pstepSize
    var iters = 0
    var deltaLL = (oldL-newL).abs
    var g = gradient.gradientAt(weights)
    while (deltaLL>threshold && iters<maxIters) {      
      g./=(g.norm(2.0)) //normalize
      val newW = weights - g*stepSize
      newL = loglikelihood(newW)
      deltaLL = (newL-oldL).abs      
      if (newL<oldL) {
        stepSize = stepSize / 2.0
      } else {
        stepSize = 1.2 * stepSize
        weights = newW
        oldL = newL
        g = gradient.gradientAt(weights)
      }
      iters+=1
    }
  }
  
  /**
   * Calculate the gradient of the NLL, to be used by optimization functions
   * cf Murphy p. 247
   */
  def gradient = 
    new DiffFunction[DenseVector[Double]] {
      def calculate(x: DenseVector[Double]) = {
        val nll = -loglikelihood(x)
        val mu_y = (inputs * x).map(sigmoid(_)) - outputs
        val res = inputstranspose*mu_y
        (nll,res+priorderiv(x))
      }    
  }

  def unregGradient =
    new DiffFunction[DenseVector[Double]] {
      def calculate(x: DenseVector[Double]) = {
        val nll = -loglikelihood(x)
        val mu_y = (inputs * x).map(sigmoid(_)) - outputs
        val res = inputstranspose*mu_y
        (nll+logprior(weights),res)
      }
  }
  
  /**
   * Builds the Hessian for the NEGATIVE log-likelihood
   * cf Murphy p. 247ff, or my own notes
   */
  def hessianAt(w: DenseVector[Double]): DenseMatrix[Double] = {
    var i=0
    var j=0
    val dim=w.size
    val H = breeze.linalg.DenseMatrix.fill(dim, dim)(0.0)
    val mu_1_mu = (inputs*w).map(sigmoidprime(_))
    def secondDeriv(x1: Int, x2: Int) = {
      var res = 0.0
      var ex = 0
      while (ex < inputs.rows) {
        res += inputs(ex,x1)*inputs(ex,x2)*mu_1_mu(ex)
        ex+=1
      }
      res
    }
    while (i<dim) {
      j=i
      while (j<dim) {
        H(i,j) = secondDeriv(i,j)
        H(j,i) = H(i,j)
        j+=1
      }
      i+=1
    }
    // with l2-regularizer
    H + breeze.linalg.DenseMatrix.eye[Double](dim) //*1000.0
  }
  
  /**
   * Calculate Hessian of NEGATIVE loglikelihood using Matrix-multiplications
   * Due to inefficiencies in Matrices, seems to not work for larger data-sets...
   */
  @deprecated(message="Too memory intensive for larger data-sets",since="")
  def hessianAtMatrix(w: DenseVector[Double]): DenseMatrix[Double] = {
    val mu_1_mu = (inputs * w).map(sigmoidprime(_))
    val S = breeze.linalg.diag(mu_1_mu)
    inputstranspose*S*inputs +  breeze.linalg.Matrix.ones[Double](w.size,w.size)
  }
  
  /**
   * calculate the (positive) loglikelihood of the (output/input)-data given a weight vector w
   * unregularized
   */
  def loglikelihood(w: DenseVector[Double]=weights): Double = {
    var res: Double = 0
    var i = 0
    if (!hasData)
      0
    else {
	    while (i<inputs.rows) {
	      val xi = inputstranspose(::,i)
	      val mu = w dot xi
	      res = res + outputs(i)*mu - math.log(1+math.exp(mu))
	      i+=1
	    }
	    val lp = logprior(w)
	    res + lp
    }
  }
  
}