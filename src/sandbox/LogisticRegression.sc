package sandbox
/**
 * Logistic Regression model
 *
 */
import scala.collection.mutable.ArrayBuffer
import npbayes.maxent
import npbayes.distributions.Categorical
import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector

 
object LogisticRegression {
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
    }                                             //> turnIntoMatrix: (in: Array[Array[Double]])breeze.linalg.DenseMatrix[Double]
  
  def features2(x: String): Array[Double] = {
    val parsed = x.split(" ")
    val temp = Array.fill[Double](3)(0.0)
    temp(0) = parsed(6) match {
    					  case "Vwl" => 1.0
    					  case _ => 0.0
    				}
    temp(1) = parsed(6) match {
    					  case "Cons" => 1.0
    					  case _ => 0.0
    				}
    temp(2) = parsed(6) match {
    						case "Pause" => 1.0
    						case _ => 0.0
    				}
    temp
  }                                               //> features2: (x: String)Array[Double]
  
  def features1(x: String): Array[Double] = {
    val parsed = x.split(" ")
    val temp = Array.fill[Double](6)(0.0)
    
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
    temp(5) = math.log(parsed(8).toInt)
    temp
  }                                               //> features1: (x: String)Array[Double]
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
  
  val s = scala.io.Source.fromFile("/home/bborschi/research/np-bayes/untracked/big.csv")
                                                  //> s  : scala.io.BufferedSource = non-empty iterator
//  val s = scala.io.Source.fromFile("/home/bborschi/research/np-bayes/untracked/toy.csv")
  val toyIn = new ArrayBuffer[String]             //> toyIn  : scala.collection.mutable.ArrayBuffer[String] = ArrayBuffer()
  val toyObs = new ArrayBuffer[Double]            //> toyObs  : scala.collection.mutable.ArrayBuffer[Double] = ArrayBuffer()
  //word drop prevSeg prevCoarse prevFine nextSeg nextCoarse nextFine freq
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
  def const(x: DenseVector[Double]) = 0.0         //> const: (x: breeze.linalg.DenseVector[Double])Double
  val l = new maxent.LogisticRegression[String](3,features2)
                                                  //> l  : npbayes.maxent.LogisticRegression[String] = npbayes.maxent.LogisticReg
                                                  //| ression@61245c4f
  l.setInputs(X)
  l.setOutputs(Y)
     

  val zero = l.weights                            //> zero  : breeze.linalg.DenseVector[Double] = DenseVector(0.0, 0.0, 0.0)
  npbayes.Utils.time(l.mapLBFGS(zero))            //> time: 2044.737063ms
  l.loglikelihood(l.weights)                      //> res0: Double = -23507.965961716196
  l.weights                                       //> res1: breeze.linalg.DenseVector[Double] = DenseVector(-1.2862729789490261, 
                                                  //| -0.5438148343219444, -1.8430377965350306)
  
  npbayes.Utils.time(l.mapGradientDescent(zero))  //> # loglikelihood after 35 iterations: -23507.968345147514 (deltaLL: 2.750221
                                                  //| 0286911577E-6)
                                                  //| time: 506.132121ms
  l.loglikelihood()                               //> res2: Double = -23507.968345147514
  l.weights                                       //> res3: breeze.linalg.DenseVector[Double] = DenseVector(-1.2871515429903362, 
                                                  //| -0.5442814771060918, -1.8441145966004293)

  npbayes.Utils.time(l.mapNewtonsMethod(zero))    //> time: 525.211347ms
  l.loglikelihood()                               //> res4: Double = -23507.96624948633
  l.weights                                       //> res5: breeze.linalg.DenseVector[Double] = DenseVector(-1.2861948401027414, 
                                                  //| -0.543827329418325, -1.843318271073049)
}