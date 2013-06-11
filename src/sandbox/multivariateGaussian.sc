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
 def featuresPNInter(x: String): Array[Double] = {
    val parsed = x.split(" ")
    val temp = Array.fill[Double](11)(0.0)
    
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
    temp(5) = (parsed(3),parsed(6)) match {
    					  case ("Cons","Cons") => 1.0
    					  case _ => 0.0
    }
		temp(6) = (parsed(3),parsed(6)) match {
    					  case ("Cons","Vwl") => 1.0
    					  case _ => 0.0
    }
		temp(7) = (parsed(3),parsed(6)) match {
    					  case ("Cons","Pause") => 1.0
    					  case _ => 0.0
    }
    temp(8) = (parsed(3),parsed(6)) match {
    					  case ("Vwl","Cons") => 1.0
    					  case _ => 0.0
    }
		temp(9) = (parsed(3),parsed(6)) match {
    					  case ("Vwl","Vwl") => 1.0
    					  case _ => 0.0
    }
		temp(10) = (parsed(3),parsed(6)) match {
    					  case ("Vwl","Pause") => 1.0
    					  case _ => 0.0
    }
    
    temp
  }                                               //> featuresPNInter: (x: String)Array[Double]
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
  
  val l = new maxent.LogisticRegression[String](11,featuresPNInter,maxent.LogisticRegression.l1prior,maxent.LogisticRegression.l2derivative)
                                                  //> l  : npbayes.maxent.LogisticRegression[String] = npbayes.maxent.LogisticReg
                                                  //| ression@22967f8e
  l.setInputs(X)
  l.setOutputs(Y)
  val zeros = l.weights                           //> zeros  : breeze.linalg.DenseVector[Double] = DenseVector(0.0, 0.0, 0.0, 0.0
                                                  //| , 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
  val test = new breeze.optimize.OWLQN[DenseVector[Double]](1000,6,750.0,0.0)
                                                  //> test  : breeze.optimize.OWLQN[breeze.linalg.DenseVector[Double]] = breeze.o
                                                  //| ptimize.OWLQN@36c0ae96
//  val H_inv0 = breeze.linalg.inv(l.hessianAt(zeros))
//  l.mapLBFGS()
//  l.mapGradientDescent(threshold=0.0,maxIters=1000)
  val map = test.minimize(l.unregGradient,l.weights) //l.weights
                                                  //> map  : breeze.linalg.DenseVector[Double] = DenseVector(-1.2912046317862718,
                                                  //|  1.1215957230131146E-6, -0.06588514220625868, 0.05256410754686743, -0.26274
                                                  //| 285138477943, 0.0861927090786544, 0.0, -1.909114627487227E-4, -0.0, -0.0234
                                                  //| 97216601268387, -0.10159162923018716)
 //map  : breeze.linalg.DenseVector[Double] = DenseVector(-1.2884896207284036, -28232.577811362447
  println(l.loglikelihood(map))                   //> -21505.20172934103
  val H_inv = breeze.linalg.inv(l.hessianAt(map)) //> H_inv  : breeze.linalg.DenseMatrix[Double] = 0.375118069808161     0.124940
                                                  //| 15973556592   ... (11 total)
                                                  //| 0.1249401597355634    0.3751931897864525    ...
                                                  //| -0.1666509872267314   -0.16661719802039482  ...
                                                  //| -0.1666666044912388   -0.16668176987823258  ...
                                                  //| -0.16662417871788932  -0.1665676823014523   ...
                                                  //| 0.0417056525341178    -0.20840751899680976  ...
                                                  //| 0.0416477519035
                                                  //| Output exceeds cutoff limit.
/*  val gaussianBad =
  new org.apache.commons.math3.distribution.MultivariateNormalDistribution(zeros.data,toArray(H_inv0))*/
  val gaussian =
  new org.apache.commons.math3.distribution.MultivariateNormalDistribution(map.data,toArray(H_inv*(math.pow(2.38, 2)/map.data.length)))
                                                  //> gaussian  : org.apache.commons.math3.distribution.MultivariateNormalDistrib
                                                  //| ution = org.apache.commons.math3.distribution.MultivariateNormalDistributio
                                                  //| n@ca5412c

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
  proplpdf(prop(null),zeros.data)                 //> res1: Double = 11.08148775059146
  
  val posterior = new java.io.PrintStream(new java.io.File("/home/bborschi/research/np-bayes/untracked/acl_13features_l1priorMurphyMixMH.txt"),"utf-8")
                                                  //> posterior  : java.io.PrintStream = java.io.PrintStream@76fe005b
  val samples=optimizer.samplersMultivariate.mhsample(map.data, logpdf, proplpdf, prop, 1000, 1000,10,true)
                                                  //> rejrate: 0.99872625
                                                  //| samples  : Array[Array[Double]] = Array(Array(-1.3240548856369794, -0.12166
                                                  //| 13740091415, 0.5657187389564846, 0.013015225841847902, -0.4151850887035139,
                                                  //|  0.24698086879254003, -0.5079227003250216, 0.239628443027968, 0.10265002670
                                                  //| 992557, -0.6760378827442559, 0.013728471284008448), Array(-1.32405488563697
                                                  //| 94, -0.1216613740091415, 0.5657187389564846, 0
                                                  //| Output exceeds cutoff limit.|
    
  for (x<-samples) {
    for (i <- x)
      posterior.printf(i+" ")
    posterior.printf(l.loglikelihood(new DenseVector(x))+"\n")
  }
  posterior.close()

  
}