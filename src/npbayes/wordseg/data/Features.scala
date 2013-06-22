package npbayes.wordseg.data

import npbayes.WordType
import scala.collection.mutable.ArrayBuffer

object Features {

  def featureDifference(fs: Set[Int], bs: Set[Int]) =
    if (fs.size>bs.size) (fs--bs).size else (bs--fs).size
  
  val featuresNo = (1, {
    val res = Array.fill[Double](1)(1.0) 
    def x(w1w2: (WordType,WordType)): Array[Double] = res
    x(_)
  })
   
  
  val featuresPNInteraction = (11, {def x(w1w2: (WordType,WordType)): Array[Double] = {
	    val (w1,w2) = w1w2
	    val res = Array.fill[Double](11)(0.0)
	    val w1prev = PhonemeClassMap.getClass(w1(w1.size-2))
	    val w2first = PhonemeClassMap.getClass(w2(0))
	    val interaction = (SymbolClassTable(w1prev),SymbolClassTable(w2first))
	    res(0) = interaction match {
	  					  case ("CONS","CONS") => 1.0
	  					  case _ => 0.0
	    		 }
	    res(1) = interaction match {
	  					  case ("CONS","VOWL") => 1.0
	  					  case _ => 0.0
	    		 } 	    
	    res(2) = interaction match {
	  					  case ("CONS","SIL") => 1.0
	  					  case _ => 0.0
	    		 } 	    
	    res(3) = (SymbolClassTable(w1prev),SymbolClassTable(w2first)) match {
	  					  case ("VOWL","CONS") => 1.0
	  					  case _ => 0.0
	    		 }
	    res(4) = (SymbolClassTable(w1prev),SymbolClassTable(w2first)) match {
	  					  case ("VOWL","VOWL") => 1.0
	  					  case _ => 0.0
	    		 }        	    
	    res(5) = (SymbolClassTable(w1prev),SymbolClassTable(w2first)) match {
	  					  case ("VOWL","SIL") => 1.0
	  					  case _ => 0.0
	    		 }        	    
	    res(6) = SymbolClassTable(w1prev) match {
	  					  case "CONS" => 1.0
	  					  case _ => 0.0
	    		 }
	    res(7) = SymbolClassTable(w1prev) match {
	  					  case "VOWL" => 1.0
	  					  case _ => 0.0
	    		 }    
	    res(8) = SymbolClassTable(w2first) match {
	  					  case "CONS" => 1.0
	  					  case _ => 0.0
	    		 }    
	    res(9) = SymbolClassTable(w2first) match {
	  					  case "VOWL" => 1.0
	  					  case _ => 0.0
	    		 }        
	    res(10) = SymbolClassTable(w2first) match {
	  					  case "SIL" => 1.0
	  					  case _ => 0.0
	    		 }  	    
	    
	    res
   	}
    x(_)}
  )
  
    val featuresInteraction = (6, {def x(w1w2: (WordType,WordType)): Array[Double] = {
	    val (w1,w2) = w1w2
	    val res = Array.fill[Double](6)(0.0)
	    val w1prev = PhonemeFeatureMap(w1(w1.size-2))
	    val w2first = PhonemeFeatureMap(w2(0))
	    
	    val cons = PhonemeFeatureMap.fmapStoN("cons")
	    val syll = PhonemeFeatureMap.fmapStoN("syll")
	    val sil = PhonemeFeatureMap.fmapStoN("sile")	    
	    val interaction = (w1prev(cons),w1prev(syll),w2first(cons),w2first(syll),w2first(sil))
	    res(0) = interaction match {
	  					  case (true,false,true,false,false) => 1.0
	  					  case _ => 0.0
	    		 } 
	    res(1) =  interaction match {
	  					  case (true,false,false,true,false) => 1.0
	  					  case _ => 0.0
	    		 }        
	    res(2) = interaction  match {
	  					  case (true,false,false,false,true) => 1.0
	  					  case _ => 0.0
	    		 }
	    res(3) = interaction match {
	  					  case (false,true,true,false,false) => 1.0
	  					  case _ => 0.0
	    		 }    
	    res(4) = interaction match {
	  					  case (false,true,false,true,false) => 1.0
	  					  case _ => 0.0
	    		 }        
	    res(5) = interaction match {
	  					  case (false,true,false,false,true) => 1.0
	  					  case _ => 0.0
	    		 }        	    
	    res
   	}
    x(_)}
  )
  
  val featuresCoetzee = (4, {def x(w1w2: (WordType,WordType)): Array[Double] = {
    val (w1,w2) = w1w2
    val res = Array.fill[Double](4)(0.0)
    val w1prev = PhonemeClassMap.getClass(w1(w1.size-2))
    val w2first = PhonemeClassMap.getClass(w2(0))
    res(0) = SymbolClassTable(w1prev) match {
      case "CONS" => -1.0
      case _ => 0.0
    }
    res(1) = -1.0 //this is a vector for predicting deletion...==> surface will have one less segment
    res(2) = SymbolClassTable(w2first) match {
      case "VOWL" => -1.0
      case _ => 0.0
    }
    res(3) = SymbolClassTable(w2first) match {
      case "SIL" => -1.0
      case _ => 0.0
    }    
    res
  } 
  x(_)  
  })
  
  val featuresPNIdent = (4, {def x(w1w2: (WordType,WordType)): Array[Double] = {
	    val (w1,w2) = w1w2
	    val res = Array.fill[Double](4)(0.0)
	    val w1prev = PhonemeClassMap.getClass(w1(w1.size-2))
	    val w2first = PhonemeClassMap.getClass(w2(0))
	    
	    res(0) = 1.0
	    res(1) = SymbolClassTable(w1prev) match {
	  					  case "CONS" => 1.0
	  					  case _ => 0.0
	    		 }    
	    res(2) = SymbolClassTable(w2first) match {
	  					  case "CONS" => 1.0
	  					  case _ => 0.0
	    		 }        
	    res(3) = SymbolClassTable(w2first) match {
	  					  case "SIL" => 1.0
	  					  case _ => 0.0
	    		 }        
	    res
   	}
    x(_)}
  )
  
  /**
    * coarse previous-next feature set 
    * (prevVwl, prevCons, nextVwl, nextCons, nextPaus)
    **/ 
   val featuresPNold = (5, {def x(w1w2: (WordType,WordType)): Array[Double] = {
	    val (w1,w2) = w1w2
	    val res = Array.fill[Double](5)(0.0)
	    val w1prev = PhonemeClassMap.getClass(w1(w1.size-2))
	    val w2first = PhonemeClassMap.getClass(w2(0))
	    
	    res(0) = SymbolClassTable(w1prev) match {
	  					  case "VOWL" => 1.0
	  					  case _ => 0.0
	    		 }
	    res(1) = SymbolClassTable(w1prev) match {
	  					  case "CONS" => 1.0
	  					  case _ => 0.0
	    		 }    
	    res(2) = SymbolClassTable(w2first) match {
	  					  case "VOWL" => 1.0
	  					  case _ => 0.0
	    		 }    
	    res(3) = SymbolClassTable(w2first) match {
	  					  case "CONS" => 1.0
	  					  case _ => 0.0
	    		 }        
	    res(4) = SymbolClassTable(w2first) match {
	  					  case "SIL" => 1.0
	  					  case _ => 0.0
	    		 }        
	    res
   	}
    x(_)
   }) 

   val featuresPN = (5, {def x(w1w2: (WordType,WordType)): Array[Double] = {
	    val (w1,w2) = w1w2
	    val res = Array.fill[Double](5)(0.0)
	    val w1prev = PhonemeFeatureMap(w1(w1.size-2))
	    val w2first = PhonemeFeatureMap(w2(0))
	    
	    val cons = PhonemeFeatureMap.fmapStoN("cons")
	    val syll = PhonemeFeatureMap.fmapStoN("syll")
	    val sil = PhonemeFeatureMap.fmapStoN("sile")
	    
	    
	    res(0) = w1prev(syll) match {
	  					  case true => 1.0
	  					  case _ => 0.0
	    		 }
	    res(1) = w1prev(cons) match {
	  					  case true => 1.0
	  					  case _ => 0.0
	    		 }    
	    res(2) = w2first(syll) match {
	  					  case true => 1.0
	  					  case _ => 0.0
	    		 }    
	    res(3) = w2first(cons) match {
	  					  case true => 1.0
	  					  case _ => 0.0
	    		 }        
	    res(4) = w2first(sil) match {
	  					  case true => 1.0
	  					  case _ => 0.0
	    		 }        
	    res
   	}
    x(_)
   })    
   
   val featureLargeName = { 
      val res = new ArrayBuffer[String]
      var fN = 0
      while (fN<PhonemeFeatureMap.nFeatures) {
        res+="prevIs_"+PhonemeFeatureMap.fmapNtoS(fN)
        res+="nextIs_"+PhonemeFeatureMap.fmapNtoS(fN)
        res+="prevLastMatch_"+PhonemeFeatureMap.fmapNtoS(fN)
        res+="nextLastMatch_"+PhonemeFeatureMap.fmapNtoS(fN)
        fN+=1
      }
      for (pN <- PhonemeFeatureMap.pmap.keySet) {
        res+= "prevIs_"+SymbolSegTable(pN)
        res+= "nextIs_"+SymbolSegTable(pN)
      }
	  res+="difPrevLast"
	  res+="difNextLast"
	  res
    }
   val featureLargeName2 = { 
      val res = new ArrayBuffer[String]
      res+="base"
      var fN = 0
      while (fN<PhonemeFeatureMap.nFeatures) {
        res+="prevIs_"+PhonemeFeatureMap.fmapNtoS(fN)
        res+="nextIs_"+PhonemeFeatureMap.fmapNtoS(fN)
        fN+=1
      }
      for (pN <- PhonemeFeatureMap.pmap.keySet) {
        res+= "prevIs_"+SymbolSegTable(pN)
        res+= "nextIs_"+SymbolSegTable(pN)
      }
	  res+="difPrevLast"
	  res+="difNextLast"
	  res
    }  
   	
   def prettyPrintWeights(w: Array[Double],names: ArrayBuffer[String]) = {
     var fN=0
     val res = new ArrayBuffer[String]
     while (fN<w.length) {
       res.+=(names(fN)+" "+w(fN))
       fN+=1
     }
     res.mkString(" ")
   }
  
   val featuresLarge2 = ({def n() = {1+2*PhonemeFeatureMap.nFeatures+2*PhonemeFeatureMap.pmap.size+2} 
   						 n},  
   						 {def x (w1w2: (WordType,WordType)): Array[Double] = {
						    val res = new ArrayBuffer[Double]
						    val (w1,w2) = w1w2
						    val w1last = PhonemeFeatureMap(w1.lastSeg)
						    val w1prec = PhonemeFeatureMap(w1(w1.size-2))
						    val w2first = PhonemeFeatureMap(w2(0))
						    //just instantiate all simple left / right
						    var fN = 0
						    res+=1.0
						    while (fN<PhonemeFeatureMap.nFeatures) {
						      res+={if (w1prec(fN)) 1.0 else 0.0}
						      res+={if (w2first(fN)) 1.0 else 0.0}
						      fN+=1
						    }
						    for (pN <- PhonemeFeatureMap.pmap.keySet) {
						    	res+= {if (w1(w1.size-2)==pN) 1.0 else 0.0}
						    	res+= {if (w2(0)==pN) 1.0 else 0.0}
						    }
						    res+=featureDifference(w1last, w1prec)
						    res+=featureDifference(w1last, w2first)
						    res.toArray
						  }
						   x(_)
    })
   
   
   val featuresLarge = ({def n() = {4*PhonemeFeatureMap.nFeatures+2*PhonemeFeatureMap.pmap.size+2} 
   						 n},  
   						 {def x (w1w2: (WordType,WordType)): Array[Double] = {
						    val res = new ArrayBuffer[Double]
						    val (w1,w2) = w1w2
						    val w1last = PhonemeFeatureMap(w1.lastSeg)
						    val w1prec = PhonemeFeatureMap(w1(w1.size-1))
						    val w2first = PhonemeFeatureMap(w2(0))
						    //just instantiate all simple left / right
						    var fN = 0
						    while (fN<PhonemeFeatureMap.nFeatures) {
						      res+={if (w1prec(fN)) 1.0 else 0.0}
						      res+={if (w2first(fN)) 1.0 else 0.0}
						      res+={if (w1prec(fN)==w1last(fN)) 1.0 else 0.0}
						      res+={if (w1last(fN)==w2first(fN)) 1.0 else 0.0}
						      fN+=1
						    }
						    for (pN <- PhonemeFeatureMap.pmap.keySet) {
						    	res+= {if (w1(w1.size-1)==pN) 1.0 else 0.0}
						    	res+= {if (w2(0)==pN) 1.0 else 0.0}
						    }
						    res+=featureDifference(w1last, w1prec)
						    res+=featureDifference(w1last, w2first)
						    res.toArray
						  }
						   x(_)
    })

   val featuresLargeNext = ({def n() = {2*PhonemeFeatureMap.nFeatures+1} 
					 n},  
					 {def x (w1w2: (WordType,WordType)): Array[Double] = {
					    val res = new ArrayBuffer[Double]
					    val (w1,w2) = w1w2
					    val w1last = PhonemeFeatureMap(w1.lastSeg)
					    val w1prec = PhonemeFeatureMap(w1(w1.size-1))
					    val w2first = PhonemeFeatureMap(w2(0))
					    //just instantiate all simple left / right
					    var fN = 0
					    while (fN<PhonemeFeatureMap.nFeatures) {
					      res+={if (w2first(fN)) 1.0 else 0.0}
					      res+={if (w1last(fN)==w2first(fN)) 1.0 else 0.0}
					      fN+=1
					    }
					    res+=featureDifference(w1last, w2first)
					    res.toArray
					  }
					   x(_)
})
  /**
    * coarse next feature set 
    * (nextVwl, nextCons, nextPaus)
    **/ 
   val featuresN = (3, {def x(w1w2: (WordType,WordType)): Array[Double] = {
	    val (w1,w2) = w1w2
	    val res = Array.fill[Double](3)(0.0)
	    val w2first = PhonemeClassMap.getClass(w2(0))
	    res(0) = SymbolClassTable(w2first) match {
	  					  case "VOWL" => 1.0
	  					  case _ => 0.0
	    		 }    
	    res(1) = SymbolClassTable(w2first) match {
	  					  case "CONS" => 1.0
	  					  case _ => 0.0
	    		 }        
	    res(2) = SymbolClassTable(w2first) match {
	  					  case "SIL" => 1.0
	  					  case _ => 0.0
	    		 }        
	    res
   	}
   x(_)
  })
}