package npbayes.maxent

import breeze.linalg
import breeze.optimize.DiffFunction
import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix
import scala.util.Random.nextDouble
import breeze.linalg.Matrix

trait FeatureVector[T] {
  def features(x: T)
}


object LogisticRegression {
    
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
     * a Gaussian prior, this is the loglikelihood, not the negative log-likelihood
     */
    def l2prior(w: DenseVector[Double]): Double = {
      -(w map (math.pow(_,2))).sum
    }
}

/**
 * binary logistic regression
 */
class LogisticRegression[Input](nfeatures: Int,val features: Input => Array[Double],val logprior: DenseVector[Double]=>Double=LogisticRegression.l2prior)  {
  
  def sigmoid(x: Double) = 1/(1+math.exp(-x))
  def sigmoidprime(x: Double) = sigmoid(x)*(1-sigmoid(x))
  
  var weights: DenseVector[Double] = DenseVector.fill(nfeatures)(0.0)
  
  var inputs: DenseMatrix[Double] = null	// N x |w| data matrix
  var inputstranspose: DenseMatrix[Double] = null //|w| x N data matrix
  var outputs: DenseVector[Double] = null // Nx1 output vector
  
  /*
   * featurize the inputs
   */
  def setInputs(in: Array[Input]) = {
    inputs = LogisticRegression.turnIntoMatrix(in map (features(_)))
    inputstranspose=inputs.t
  }
  
   
  def setOutputs(out: Array[Double]) = {
    outputs = DenseVector(out)
  }
  
 
  
  /**
   * perform MAP-inference using LBFGS
   */
  def mapLBFGS(start: DenseVector[Double]=weights,maxIters:Int=100,m: Int=3) = {
    val lbfgs = new breeze.optimize.LBFGS[DenseVector[Double]](maxIters,m)
    weights = lbfgs.minimize(gradient, start)
  }
  
  
  /**
   * see HelmsHold2006
   */
  def helmsHoldGibbs(start: DenseVector[Double]=weights) = {
	  
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
    //println("# loglikelihood at start: "+oldL)
    while (deltaLL>threshold && iters<maxIters) {
      val g = gradient.gradientAt(weights)
      val H = hessianAt(weights)
      val H_inv = breeze.linalg.inv(hessianAt(weights))
      val d = H_inv*g
      val newW = weights - d*stepSize
      newL = loglikelihood(newW)
      deltaLL = (newL-oldL).abs
      if (newL<oldL) {
        stepSize = stepSize / 2.0
        //println("half step-size (now "+stepSize+")")
      } else {
        stepSize = 1.2 * stepSize
    	//println("increase step-size (now "+stepSize+")")
        weights = newW        
        oldL = newL
      }
//      println(oldL+" "+weights)
      iters+=1      
    }
//    println("# loglikelihood after "+iters+" iterations: "+oldL+" (deltaLL "+deltaLL+")")    
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
//    println("# loglikelihood at start: "+oldL)
    while (deltaLL>threshold && iters<maxIters) {
      val g = gradient.gradientAt(weights)
      g./=(g.norm(2.0)) //normalize
      val newW = weights - g*stepSize
      newL = loglikelihood(newW)
      deltaLL = (newL-oldL).abs      
      if (newL<oldL) {
        stepSize = stepSize / 2.0
        //println("half step-size (now "+stepSize+")")
      } else {
        stepSize = 1.2 * stepSize
    	//println("increase step-size (now "+stepSize+")")
        weights = newW
        oldL = newL
      }
      iters+=1
    }
    println("# loglikelihood after "+iters+" iterations: "+oldL+" (deltaLL: "+deltaLL+")")
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
        (nll,res+x) //l2-regularizer, make generic for differentiable priors
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
    H + breeze.linalg.Matrix.ones[Double](dim,dim)
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
    while (i<inputs.rows) {
      val xi = inputstranspose(::,i)
      val mu = w dot xi
      res = res + outputs(i)*mu - math.log(1+math.exp(mu))
      i+=1
    }
    res + logprior(w)
  }
  
}