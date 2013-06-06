package sandbox
import scala.collection.mutable.ArrayBuffer
import npbayes.maxent
import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector

object multivariateGaussian {

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
  }                                               //> toArray: (in: breeze.linalg.DenseMatrix[Double])Array[Array[Double]]
 def featuresPN(x: String): Array[Double] = {
    val parsed = x.split(" ")
    val temp = Array.fill[Double](5)(0.0)
    
    temp(0) = parsed(3) match {
    					  case "Cons" => 0.0
    					  case "Vwl" => 1.0
    				}
    temp(1) = parsed(3) match {
    					  case "Cons" => 1.0
    					  case "Vwl" => 0.0
    				}
    temp(2) = parsed(6) match {
    					  case "Cons" => 0.0
    					  case "Vwl" => 1.0
    					  case "Pause" => 0.0
    				}
    temp(3) = parsed(6) match {
    					  case "Cons" => 1.0
    					  case "Vwl" => 0.0
    					  case "Pause" => 0.0
    				}
    temp(4) = parsed(6) match {
    					  case "Cons" => 0.0
    					  case "Vwl" => 0.0
    					  case "Pause" => 1.0
    				}
    temp
  }                                               //> featuresPN: (x: String)Array[Double]
  def features(x: String): Array[Double] = {
    val parsed = x.split(" ")
    val temp = Array.fill[Double](13)(0.0)
    
    temp(0) = parsed(4) match {
    					  case "fricative" => 1.0
    					  case _ => 0.0
    				}
    temp(1) = parsed(4) match {
    					  case "vowel" => 1.0
    					  case _ => 0.0
    				}
    temp(2) = parsed(4) match {
    					  case "approximant" => 1.0
    					  case _ => 0.0
    				}
    temp(3) = parsed(4) match {
    					  case "nasal" => 1.0
    					  case _ => 0.0
    				}
    temp(4) = parsed(4) match {
    					  case "stop" => 1.0
    					  case _ => 0.0
    				}
	  temp(5) = parsed(4) match {
	  						case "affricate" => 1.0
	  						case _ => 0.0
	  }
    temp(6) = parsed(7) match {
    					  case "fricative" => 1.0
    					  case _ => 0.0
    				}
    temp(7) = parsed(7) match {
    					  case "vowel" => 1.0
    					  case _ => 0.0
    				}
    temp(8) = parsed(7) match {
    					  case "approximant" => 1.0
    					  case _ => 0.0
    				}
    temp(9) = parsed(7) match {
    					  case "nasal" => 1.0
    					  case _ => 0.0
    				}
    temp(10) = parsed(7) match {
    					  case "stop" => 1.0
    					  case _ => 0.0
    				}
	  temp(11) = parsed(7) match {
	  						case "affricate" => 1.0
	  						case _ => 0.0
	  }
	  temp(12) = parsed(7) match {
	  						case "Pause" => 1.0
	  						case _ => 0.0
	  }
	  temp
  }                                               //> features: (x: String)Array[Double]

	def twofeatures(x: String): Array[Double] = {
    val parsed = x.split(" ")
    val temp = Array.fill[Double](2)(0.0)
    temp(0) = parsed(6) match {
    					  case "Vwl" | "Pause" => 1.0
    					  case _ => 0.0
    				}
    temp(1) = parsed(6) match {
    					  case "Cons" => 1.0
    					  case _ => 0.0
    				}
    temp
  }                                               //> twofeatures: (x: String)Array[Double]
  
  def threefeatures(x: String): Array[Double] = {
    val parsed = x.split(" ")
    val temp = Array.fill[Double](3)(0.0)
    temp(0) = parsed(6) match {
    					  case "Vwl"  => 1.0
    					  case _ => 0.0
    				}
    temp(1) = parsed(6) match {
    					  case "Cons" => 1.0
    					  case _ => 0.0
    				}
    temp (2) = parsed(6) match {
    						case "Pause" => 1.0
    						case _ => 0.0
    				}
    temp
	  }                                       //> threefeatures: (x: String)Array[Double]
  
    val s = scala.io.Source.fromFile("/home/bborschi/research/np-bayes/untracked/big.csv")
                                                  //> s  : scala.io.BufferedSource = non-empty iterator
                                                
  val toyIn = new ArrayBuffer[String]             //> toyIn  : scala.collection.mutable.ArrayBuffer[String] = ArrayBuffer()
  val toyObs = new ArrayBuffer[Double]            //> toyObs  : scala.collection.mutable.ArrayBuffer[Double] = ArrayBuffer()
  for (l <- s.getLines()) {
    val parsed = l.stripLineEnd.split(" ")
    toyObs+=(parsed(1) match {
    					  case "yes" => 1.0
    					  case "no" => 0.0
    					})
    toyIn+=(l)
  }
  val Y = toyObs.toArray                          //> Y  : Array[Double] = Array(1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0
                                                  //| , 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0
                                                  //| , 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1
                                                  //| Output exceeds cutoff limit.
  val X = toyIn.toArray                           //> X  : Array[String] = Array(wehst yes s Cons fricative s Cons fricative 22, 
                                                  //| ahsowshiyeyt no ey Vwl vowel Pau Pause Pause 8, aet no ae Vwl vowel d Cons 
                                                  //| stop 967, dhaet no ae Vwl vowel P
                                                  //| Output exceeds cutoff limit.
  
  val l = new maxent.LogisticRegression[String](3,threefeatures,maxent.LogisticRegression.l2prior,maxent.LogisticRegression.l2derivative)
                                                  //> l  : npbayes.maxent.LogisticRegression[String] = npbayes.maxent.LogisticReg
                                                  //| ression@14cbd69
  l.setInputs(X)
  l.setOutputs(Y)
  val zeros = l.weights                           //> zeros  : breeze.linalg.DenseVector[Double] = DenseVector(0.0, 0.0, 0.0)
//  val H_inv0 = breeze.linalg.inv(l.hessianAt(zeros))
  l.mapLBFGS()
//  l.mapGradientDescent(threshold=0.0,maxIters=1000)
  val map = l.weights                             //> map  : breeze.linalg.DenseVector[Double] = DenseVector(-1.2862729789490261,
                                                  //|  -0.5438148343219444, -1.8430377965350306)
 //map  : breeze.linalg.DenseVector[Double] = DenseVector(-1.2884896207284036,
  
  println(l.loglikelihood())                      //> -23507.965961716196
  val H_inv = breeze.linalg.inv(l.hessianAt(map)) //> H_inv  : breeze.linalg.DenseMatrix[Double] = 5.692415690290101E-4    -1.056
                                                  //| 0219053688055E-7  -6.685926372950707E-7   
                                                  //| -1.0560219053688054E-7  1.8573205041679913E-4   -2.180647223206276
                                                  //| Output exceeds cutoff limit.
/*  val gaussianBad =
  new org.apache.commons.math3.distribution.MultivariateNormalDistribution(zeros.data,toArray(H_inv0))*/
  val gaussian =
  new org.apache.commons.math3.distribution.MultivariateNormalDistribution(map.data,toArray(H_inv*(math.pow(2.38, 2)/map.data.length)))
                                                  //> gaussian  : org.apache.commons.math3.distribution.MultivariateNormalDistrib
                                                  //| ution = org.apache.commons.math3.distribution.MultivariateNormalDistributio
                                                  //| n@3a22d561

/*  def propb(x: Array[Double]): Array[Double] = {
  	val gaussianBad =
      new org.apache.commons.math3.distribution.MultivariateNormalDistribution(x,toArray(breeze.linalg.diag(breeze.linalg.DenseVector.fill(x.size)(1.0))))
  	gaussianBad.sample()
  }
  def proplpdfb(x: Array[Double],y: Array[Double]) = {
    val gaussianBad =
      new org.apache.commons.math3.distribution.MultivariateNormalDistribution(y,toArray(breeze.linalg.diag(breeze.linalg.DenseVector.fill(x.size)(1.0))))
  	gaussianBad.sample()
    math.log(gaussianBad.density(x))
  }*/
   
  def prop(x: Array[Double]): Array[Double] = gaussian.sample()
                                                  //> prop: (x: Array[Double])Array[Double]
  def proplpdf(x: Array[Double],y: Array[Double]) = {
    val res = math.log(gaussian.density(x))
    if (res.isNegInfinity)
      Double.MinValue
    else
      res
  }                                               //> proplpdf: (x: Array[Double], y: Array[Double])Double
  def logpdf(x: Array[Double]) = l.loglikelihood(new DenseVector(x))
                                                  //> logpdf: (x: Array[Double])Double
  
  proplpdf(zeros.data,zeros.data)                 //> res0: Double = -1.7976931348623157E308
  proplpdf(prop(null),zeros.data)                 //> res1: Double = 7.027794165627795
  
  val posterior = new java.io.PrintStream(new java.io.File("/home/bborschi/research/np-bayes/untracked/acl_3_l2priorMurphyMixMH.txt"),"utf-8")
                                                  //> posterior  : java.io.PrintStream = java.io.PrintStream@4a5afb90
  val samples=optimizer.samplersMultivariate.mhsample(map.data, logpdf, proplpdf, prop, 1000, 10000,10,true)
                                                  //> rejrate: 0.38614333
                                                  //| samples  : Array[Array[Double]] = Array(Array(-1.2955966276313207, -0.51449
                                                  //| 94365988814, -1.902459064550803), Array(-1.3160089835440871, -0.52898036169
                                                  //| 51495, -
                                                  //| Output exceeds cutoff limit.-
  
  for (x<-samples) {
    for (i <- x)
      posterior.printf(i+" ")
    posterior.printf(l.loglikelihood(new DenseVector(x))+"\n")
  }
  posterior.close()

  
}