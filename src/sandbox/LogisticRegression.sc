package sandbox
/**
 * Logistic Regression model
 *
 */
import scala.collection.mutable.ArrayBuffer
import npbayes.maxent
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
                                                  //| , 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0
                                                  //| , 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 1.0
                                                  //| , 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0,
                                                  //| Output exceeds cutoff limit.
  val X = toyIn.toArray                           //> X  : Array[String] = Array(wehst yes s Cons fricative s Cons fricative 22, 
                                                  //| ahsowshiyeyt no ey Vwl vowel Pau Pause Pause 8, aet no ae Vwl vowel d Cons 
                                                  //| stop 967, dhaet no ae Vwl vowel Pau Pause Pause 4909, sowrt yes r Cons appr
                                                  //| oximant ah Vwl vowel 38, ahbawt no aw Vwl vowel ey Vwl vowel 964, dihfahrah
                                                  //| nt no n Cons nasal Pau Pause Pause 210, dihfahrahnt no n Cons nasal f C
                                                  //| Output exceeds cutoff limit.
  def const(x: DenseVector[Double]) = 0.0         //> const: (x: breeze.linalg.DenseVector[Double])Double
  val l = new maxent.LogisticRegression[String](13,features)
                                                  //> l  : npbayes.maxent.LogisticRegression[String] = npbayes.maxent.LogisticReg
                                                  //| ression@426d5af1
  l.setInputs(X)
  l.setOutputs(Y)
     

  val zero = l.weights                            //> zero  : breeze.linalg.DenseVector[Double] = DenseVector(0.0, 0.0, 0.0, 0.0,
                                                  //|  0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
  npbayes.Utils.time(l.mapLBFGS(zero))            //> Logprob at start: -28224.953192376288
                                                  //| Logprob at end: -20617.83357911706
                                                  //| time: 1453.110746ms
  l.loglikelihood(l.weights)                      // > res0: Double = -23507.988141605063
                                                  //> res0: Double = -20617.83357911706
  l.weights                                       //> res1: breeze.linalg.DenseVector[Double] = DenseVector(0.6477144065601169, -
                                                  //| 1.225648549562644, -1.3291104470072754, 0.5838775203320257, -0.391760128610
                                                  //| 4674, 0.2170581705685165, -0.2411726890072555, -0.5534509195166006, 0.10535
                                                  //| 034183525747, 0.35170229380058654, 0.16430401213752738, -0.0773112076520320
                                                  //| 6, -1.247290859319128)
  
  npbayes.Utils.time(l.mapGradientDescent(zero))  //> time: 1656.146133ms
  l.loglikelihood()                               //> res2: Double = -20617.738781665266
  l.weights                                       //> res3: breeze.linalg.DenseVector[Double] = DenseVector(0.657028389355999, -1
                                                  //| .2204176008049221, -1.2735649263109097, 0.597398332286427, -0.3849121263976
                                                  //| 585, 0.13277381733946436, -0.2489640200357967, -0.5605653673192469, 0.09505
                                                  //| 942301465581, 0.34912870919388367, 0.1589786507027115, -0.03716730523419123
                                                  //| 4, -1.2481642048540902)

  npbayes.Utils.time(l.mapNewtonsMethod(zero))    //> time: 1791.389214ms
  l.loglikelihood()                               //> res4: Double = -20617.47170420163
  l.weights                                       //> res5: breeze.linalg.DenseVector[Double] = DenseVector(0.6290058550989026, -
                                                  //| 1.247551356490247, -1.3204245029175297, 0.5693310655256005, -0.413358468981
                                                  //| 85885, 0.472820123599481, -0.22125760201498204, -0.5324646460589644, 0.1230
                                                  //| 9812191647439, 0.37702092661009917, 0.1869404692303347, -0.0227001208046081
                                                  //| 62, -1.2208144263127656)
  l.weights = zero
  npbayes.Utils.time(l.stochasticDescent(iters=1000))
                                                  //> Logprob at start: -28224.953192376288
                                                  //| Logprob at end: -20618.162003142945
                                                  //| time: 72631.59978ms
  l.loglikelihood()                               //> res6: Double = -20618.162003142945
  l.weights                                       //> res7: breeze.linalg.DenseVector[Double] = DenseVector(0.640048413311838, -1
                                                  //| .2606284926005908, -1.2916039379501192, 0.5751165682338464, -0.405334047400
                                                  //| 39874, 0.39512871415411466, -0.23569805268973207, -0.5329941379601673, 0.12
                                                  //| 610169674084945, 0.36787870666159456, 0.18214021940798863, -0.0261398172514
                                                  //| 13382, -1.228561397160774)
}