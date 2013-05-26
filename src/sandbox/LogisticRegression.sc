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
  
  def features(x: String): Array[Double] = {
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
                                                  //| , 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0
                                                  //| , 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0, 1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 1.0
                                                  //| , 0.0, 1.0, 1.0, 0.0, 0.0, 0.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0
                                                  //| , 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0
                                                  //| , 0.0, 0.0, 1.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0
                                                  //| , 0.0, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0
                                                  //| , 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 1.0, 1.0, 0.0, 0.0
                                                  //| , 0.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 0.0, 0.0, 0.0, 1.0
                                                  //| , 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0
                                                  //| , 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0, 1.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0
                                                  //| , 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0
                                                  //| , 0.0, 0.0, 0.0, 1.0, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0
                                                  //| , 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0
                                                  //| , 0.0, 0.0, 0.0, 1.0, 1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0
                                                  //| , 1.0, 0.0, 0.0, 1.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 1.0, 1.0, 0.0
                                                  //| , 0.0, 1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0
                                                  //| , 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0
                                                  //| , 0.0, 1.0, 1.0, 1.0, 1.0, 0.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0
                                                  //| , 1.0, 0.0, 1.0, 1.0, 1.0, 1.0, 0.0, 1.0, 0.0, 1.0, 1.0, 1.0, 0.0, 1.0, 1.0
                                                  //| , 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0
                                                  //| , 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0, 1.0, 0.0, 1.0, 0.0
                                                  //| , 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0
                                                  //| , 0.0, 0.0, 0.0, 1.0, 1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0
                                                  //| , 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0
                                                  //| , 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0
                                                  //| , 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 0.0, 1.0, 0.0, 0.0, 0.0
                                                  //| , 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0, 0.0
                                                  //| , 1.0, 0.0, 0.0, 1.0, 0.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0
                                                  //| , 1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0
                                                  //| , 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0
                                                  //| , 1.0, 1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 1.0, 1.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0
                                                  //| , 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0
                                                  //| , 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0
                                                  //| , 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0, 0.0
                                                  //| , 1.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 1.0
                                                  //| , 0.0, 0.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 1.0
                                                  //| , 0.0, 1.0, 0.0, 0.0, 0.0, 1.0, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0
                                                  //| , 1.0, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0
                                                  //| , 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 0.0, 0.0
                                                  //| , 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 1.0
                                                  //| , 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0
                                                  //| , 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0, 0.0
                                                  //| , 0.0, 1.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 1.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0
                                                  //| , 0.0, 0.0, 1.0, 0.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0
                                                  //| , 0.0, 0.0, 1.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0
                                                  //| , 0.0, 0.0, 1.0, 0.0, 0.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 0.0, 0.0
                                                  //| , 1.0, 1.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.0
                                                  //| , 1.0, 1.0, 1.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 1.0, 1.0
                                                  //| , 0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0
                                                  //| , 0.0, 0.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0
                                                  //| , 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0
                                                  //| , 0.0, 0.0, 0.0, 1.0, 0.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0
                                                  //| , 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0
                                                  //| , 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0
                                                  //| , 1.0, 1.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 0.0, 0.0, 0.0, 1.0, 0.0
                                                  //| , 0.0, 0.0, 0.0, 1.0, 0.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0
                                                  //| , 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0, 1.0, 0.0, 0.0, 1.0
                                                  //| , 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.
                                                  //| Output exceeds cutoff limit.
  val X = toyIn.toArray                           //> X  : Array[String] = Array(wehst yes s Cons fricative s Cons fricative 22, 
                                                  //| ahsowshiyeyt no ey Vwl vowel Pau Pause Pause 8, aet no ae Vwl vowel d Cons 
                                                  //| stop 967, dhaet no ae Vwl vowel Pau Pause Pause 4909, sowrt yes r Cons appr
                                                  //| oximant ah Vwl vowel 38, ahbawt no aw Vwl vowel ey Vwl vowel 964, dihfahrah
                                                  //| nt no n Cons nasal Pau Pause Pause 210, dihfahrahnt no n Cons nasal f Cons 
                                                  //| fricative 210, aet no ae Vwl vowel l Cons approximant 967, dhaet no ae Vwl 
                                                  //| vowel hh Cons fricative 4909, aet no ae Vwl vowel k Cons stop 967, awt no a
                                                  //| w Vwl vowel t Cons stop 998, persehnt yes n Cons nasal dh Cons fricative 34
                                                  //| , awt no aw Vwl vowel Pau Pause Pause 998, steyt no ey Vwl vowel ah Vwl vow
                                                  //| el 108, steyt no ey Vwl vowel dh Cons fricative 108, dhaet no ae Vwl vowel 
                                                  //| hh Cons fricative 4909, rayt no ay Vwl vowel aw Vwl vowel 821, sahjhehst no
                                                  //|  s Cons fricative m Cons nasal 6, baht no ah Vwl vowel dh Cons fricative 25
                                                  //| 86, waht no ah Vwl vowel ay Vwl vowel 1057, kahrehkt no k Cons stop Pau Pau
                                                  //| se Pause 7, baht no ah Vwl vowel Pau Pause Pause 2586, iht no ih Vwl vowel 
                                                  //| w Cons approximant 3510, rayt no ay Vwl vowel n Cons nasal 821, steyt no ey
                                                  //|  Vwl vowel b Cons stop 108, baht no ah Vwl vowel ay Vwl vowel 2586, downt n
                                                  //| o n Cons nasal ay Vwl vowel 571, naat no aa Vwl vowel ey Vwl vowel 1290, ge
                                                  //| ht no eh Vwl vowel aa Vwl vowel 998, baht no ah Vwl vowel Pau Pause Pause 2
                                                  //| 586, downt yes n Cons nasal ih Vwl vowel 1669, wehnt yes n Cons nasal t Con
                                                  //| s stop 225, wehnt no n Cons nasal t Cons stop 202, biht no ih Vwl vowel Pau
                                                  //|  Pause Pause 117, aet yes ae Vwl vowel aw Vwl vowel 185, downt no n Cons na
                                                  //| sal l Cons approximant 571, laat no aa Vwl vowel ah Vwl vowel 724, laat no 
                                                  //| aa Vwl vowel ah Vwl vowel 724, gaat no aa Vwl vowel r Cons approximant 622,
                                                  //|  jhahst yes s Cons fricative dh Cons fricative 1746, steyt no ey Vwl vowel 
                                                  //| f Cons fricative 108, laat no aa Vwl vowel ah Vwl vowel 724, jhahst no s Co
                                                  //| ns fricative n Cons nasal 696, geht no eh Vwl vowel ah Vwl vowel 998, jhahs
                                                  //| t yes s Cons fricative dh Cons fricative 1746, baht no ah Vwl vowel dh Cons
                                                  //|  fricative 2586, baht no ah Vwl vowel ay Vwl vowel 2586, dhaet no ae Vwl vo
                                                  //| wel Pau Pause Pause 4909, rayt no ay Vwl vowel n Cons nasal 821, dhaet no a
                                                  //| e Vwl vowel w Cons approximant 4909, downt yes n Cons nasal hh Cons fricati
                                                  //| ve 1669, baht no ah Vwl vowel Pau Pause Pause 2586, ahbawt no aw Vwl vowel 
                                                  //| k Cons stop 964, downt yes n Cons nasal th Cons fricative 1669, rayt no ay 
                                                  //| Vwl vowel Pau Pause Pause 821, rayt no ay Vwl vowel Pau Pause Pause 821, ra
                                                  //| yt no ay Vwl vowel Pau Pause Pause 821, downt yes n Cons nasal Pau Pause Pa
                                                  //| use 1669, dhaet no ae Vwl vowel m Cons nasal 4909, ahbawt no aw Vwl vowel i
                                                  //| h Vwl vowel 964, iht no ih Vwl vowel t Cons stop 3510, kwayt no ay Vwl vowe
                                                  //| l aa Vwl vowel 47, aanahst no s Cons fricative ay Vwl vowel 14, ahbawt no a
                                                  //| w Vwl vowel ih Vwl vowel 964, iht no ih Vwl vowel ih Vwl vowel 3510, dhaet 
                                                  //| no ae Vwl vowel Pau Pause Pause 4909, dhaet no ae Vwl vowel ah Vwl vowel 49
                                                  //| 09, iht yes ih Vwl vowel w Cons approximant 1087, frahnt yes n Cons nasal p
                                                  //|  Cons stop 38, luhkt no k Cons stop ae Vwl vowel 29, aet no ae Vwl vowel ih
                                                  //|  Vwl vowel 967, iht no ih Vwl vowel b Cons stop 3510, mahst yes s Cons fric
                                                  //| ative b Cons stop 18, iht no ih Vwl vowel ay Vwl vowel 3510, downt no n Con
                                                  //| s nasal n Cons nasal 571, ehkstehnt no n Cons nasal Pau Pause Pause 5, baht
                                                  //|  yes ah Vwl vowel ay Vwl vowel 390, kaent yes n Cons nasal ih Vwl vowel 152
                                                  //| , iht no ih Vwl vowel w Cons approximant 3510, naat no aa Vwl vowel n Cons 
                                                  //| nasal 1290, geht no eh Vwl vowel ay Vwl vowel 998, kaent yes n Cons nasal i
                                                  //| h Vwl vowel 152, dhaet no ae Vwl vowel ih Vwl vowel 4909, iht yes ih Vwl vo
                                                  //| wel m Cons nasal 1087, mayt no ay Vwl vowel m Cons nasal 116, jhahst yes s 
                                                  //| Cons fricative Pau Pause Pause 1746, iht yes ih Vwl vowel w Cons approximan
                                                  //| t 1087, naat no aa Vwl vowel dh Cons fricative 1290, dhaet no ae Vwl vowel 
                                                  //| ih Vwl vowel 4909, dhaet no ae Vwl vowel ah Vwl vowel 4909, downt yes n Con
                                                  //| s nasal n Cons nasal 1669, jhahst yes s Cons fricative b Cons stop 1746, ah
                                                  //| sowshiyeyt no ey Vwl vowel dh Cons fricative 8, dhaet no ae Vwl vowel w Con
                                                  //| s approximant 4909, rayt no ay Vwl vowel Pau Pause Pause 821, dhaet no ae V
                                                  //| wl vowel w Cons approximant 4909, wayahndaat no aa Vwl vowel l Cons approxi
                                                  //| mant 3, spehnt yes n Cons nasal ey Vwl vowel 13, laat yes aa Vwl vowel ah V
                                                  //| wl vowel 98, aet no ae Vwl vowel dh Cons fricative 967, dihfahrahnt no n Co
                                                  //| ns nasal s Cons fricative 210, dihfahrahnt no n Cons nasal r Cons approxima
                                                  //| nt 210, ehksahlahnt no n Cons nasal iy Vwl vowel 4, aet no ae Vwl vowel ow 
                                                  //| Vwl vowel 967, steyt no ey Vwl vowel dh Cons fricative 108, gaat no aa Vwl 
                                                  //| vowel dh Cons fricative 622, laat no aa Vwl vowel Pau 
                                                  //| Output exceeds cutoff limit.
  def const(x: DenseVector[Double]) = 0.0         //> const: (x: breeze.linalg.DenseVector[Double])Double
  val l = new maxent.LogisticRegression[String](6,features)
                                                  //> l  : npbayes.maxent.LogisticRegression[String] = npbayes.maxent.LogisticReg
                                                  //| ression@325d2406
  l.setInputs(X)
  l.setOutputs(Y)
  l.hessianAt(l.weights)                          //> res0: breeze.linalg.DenseMatrix[Double] = 7069.5             1.0           
                                                  //|       1927.75             ... (6 total)
                                                  //| 1.0                3112.5              662.0               ...
                                                  //| 1927.75            662.0               2588.75             ...
                                                  //| 3776.5             2016.5              1.0                 ...
                                                  //| 1367.25            436.0               1.0                 ...
                                                  //| 47574.70343188148  15244.155878715146  15525.654274057228  ...
//  l.hessianAtMatrix(l.weights)
  val zero = l.weights                            //> zero  : breeze.linalg.DenseVector[Double] = DenseVector(0.0, 0.0, 0.0, 0.0,
                                                  //|  0.0, 0.0)
  npbayes.Utils.time(l.gradientDescent(zero,threshold=0.000000001,maxIters=10000))
                                                  //> # loglikelihood at start: -28224.953192376288
                                                  //| # loglikelihood after 1688 iterations: -21032.319803975035
                                                  //| time: 28034.479674ms
  l.loglikelihood()                               //> res1: Double = -21032.319803975035
  l.weights                                       //> res2: breeze.linalg.DenseVector[Double] = DenseVector(-1.2323514802884596, 
                                                  //| 0.3498757253308379, -0.3337464597298733, 0.35871511383004573, -0.9074444090
                                                  //| 577866, -0.0459867227806939)
  npbayes.Utils.time(l.newtonsMethod(zero,threshold=0.00000000,maxIters=10000))
                                                  //> time: 4175.019089ms
  l.loglikelihood()                               //> res3: Double = -21032.320596448808
  l.weights                                       //> res4: breeze.linalg.DenseVector[Double] = DenseVector(-1.2337209279475025, 
                                                  //| 0.34872609819248396, -0.33387155333916785, 0.35838417776689824, -0.90810119
                                                  //| 88653767, -0.04571968116719854)
}