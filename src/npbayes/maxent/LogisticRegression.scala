package npbayes.maxent

import breeze.linalg
import breeze.optimize.DiffFunction
import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix
import scala.util.Random.nextDouble

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
     * a Gaussian prior
     */
    def l2prior(w: DenseVector[Double]): Double = {
      (w map (math.pow(_,2))).sum
    }
}

/**
 * binary logistic regression
 */
class LogisticRegression[Input](nfeatures: Int,val features: Input => Array[Double],val logprior: DenseVector[Double]=>Double=LogisticRegression.l2prior)  {
  
  def sigmoid(x: Double) = 1/(1+math.exp(-x))
  
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
   * Calculate the gradient, to be used by optimization functions
   * cf Murphy p. 247
   */
  def gradient = 
    new DiffFunction[DenseVector[Double]] {
      def calculate(x: DenseVector[Double]) = {
        val nll = -loglikelihood(x)//+logprior(x)
        val mu_y = (inputs * weights).map(sigmoid(_)) - outputs
        val res = inputstranspose*mu_y
        (nll,res+x) //l2-regularizer, make generic for differentiable priors
      }    
    
  }
  
  /**
   * calculate the loglikelihood of the (output/input)-data given a weight vector w
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
    res+logprior(w)
  }
  
}