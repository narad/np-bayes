package npbayes.wordseg.models
/**
 * Goldwater-style Unigram model
 */

import npbayes.wordseg
import npbayes.distributions._
import npbayes.wordseg.data._
import npbayes.wordseg.lexgens._
import scala.util.Random.shuffle
import npbayes.wordseg.`package`
import npbayes.wordseg.IGNOREDROP
import optimizer.SliceSampler
import npbayes.Utils
import java.util.Random
import npbayes.LexGenerator
import npbayes.UNIUNLEARNED
import npbayes.UNILEARNED
import npbayes.UNILEARNEDVOWELS
import org.apache.commons.math3.special.Gamma




abstract class UContext

case class UnigramMedialContext(val w1O: WordType, val w1U: WordType, val w1D: WordType,
					  val w2O: WordType, val w2U: WordType,
					  val w1w2O: WordType, val w1w2U: WordType,
					  val rO: WordType, val rU: WordType) extends UContext {
  override def toString: String =
    "("+wToS(w1U)+","+wToS(w1O)+") ("+wToS(w2U)+","+wToS(w2O)+") ("+wToS(rU)+","+wToS(rO)+")"
}

case class UnigramFinalContext(val wO: WordType, val wU: WordType, val wD: WordType) extends UContext {
  override def toString: String =
    "("+wToS(wU)+","+wToS(wO)+") ($)"  
}					  
					  

class Unigram(val corpusName: String,concentration: Double,discount: Double=0,val pWB: Double = 0.5, val assumption: HEURISTIC = EXACT,val dropSeg: String = "KLRK", val dropInd: String = "KLRK", val dropProb: Double =0.0,
				val contextModel: DeletionModel, val lexgen: LexGenerator) extends WordsegModel {
	require(0<=discount && discount<1)
	require(if (discount==0) concentration>0 else concentration>=0)
	val betaUB = 2.0
	val data = new VarData(corpusName,dropProb,dropInd,dropSeg,"0","0",contextModel)
	//nSymbols-2 because of the "$" and the drop-indicator symbol
	//val pypUni = new CRP[WordType](concentration,discount,new MonkeyUnigram(SymbolTable.nSymbols-2,0.5),assumption)
	val pypUni = {
	  val tlexgen = lexgen match {
	    case UNIUNLEARNED =>
	    	new MonkeyUnigram(npbayes.wordseg.data.SymbolTable.nSymbols-2,0.5)
	    case UNILEARNED =>
	       new UnigramLearned(npbayes.wordseg.data.SymbolTable.nSymbols-2,0.1)
	    case UNILEARNEDVOWELS =>
	       new UnigramLearned(npbayes.wordseg.data.SymbolTable.nSymbols-2,0.1)
	    case _ =>
	      throw new Error("invalid value for lexgen: "+lexgen)
	  }
	  new CRP[WordType](concentration,discount,tlexgen,assumption)	
	}
	
	val unif = new Random
	
	var nUtterances = 0 
	var changed = 0
	var boundToNo = 0
	var noToBound = 0
	
	
	def evaluate =
	  data.evaluate.toString
	
	def boundaries = data.boundaries
	def nTokens = pypUni._oCount

	def update(w: WordType) = pypUni.update(w)

	def remove(w: WordType)= pypUni.remove(w)

	/**
	 * make sure distortion only happens when we actually use a model with drops
	 */
	def toSurface(u: WordType,s: WordType, r:WordType): Double = {
	  if (wordseg.wordseg.dropInferenceMode==IGNOREDROP) {
	    if (u==s)
	      1.0
	    else
	      0.0
	  } else {
	    data.R(u, s, r)
	  }
	}
	  
	def DROP1 = data.DROP1
	def _phoneSeq = data.data
	
	
	/**
	 * returns the probability for generating an utterance final word
	 */
	def _predBoundary(phantomCustomers: Int=0) = {
	  (nUtterances+betaUB/2.0)/(nTokens+betaUB+phantomCustomers)
	}
	   
	
	/**
	 * initializes the CRP with the counts
	 */
	def init(gold:Boolean = false, binitProb:Double = 0.5) = {
	  def ungoldType(bounds: Array[Boundary], pos: Int): Unit = {
	     if (pos==bounds.size)
	      Unit
	    else {
	      bounds(pos) match {
	      	case UBoundaryDrop1 | UBoundaryNoDrop1 => 
	      	  if (data._random.nextDouble<=binitProb)
	      		  bounds(pos)=UBoundaryDrop1
	      	  else
	      		   bounds(pos)=UBoundaryNoDrop1
	      	case WBoundaryDrop1 | WBoundaryNoDrop1 => 
	      	  if (data._random.nextDouble<=binitProb)
	      	    bounds(pos)=WBoundaryDrop1
	      	  else
	      	    bounds(pos)=WBoundaryNoDrop1
	      	case _ =>
	      }
	      ungoldType(bounds,pos+1)
	    }
	  }
	  def undrop(data: Array[Boundary], pos: Int): Unit = {
	    if (pos==data.size)
	      Unit
	    else {
	      data(pos) match {
	      	case UBoundaryDrop1 => data(pos)=UBoundaryNoDrop1
	      	case WBoundaryDrop1 => data(pos)=WBoundaryNoDrop1
	      	case _ =>
	      }
	      undrop(data,pos+1)
	    }
	  }	  
	  def inner(sPos: Int,cPos: Int): Unit = 
	    if (cPos>=boundaries.length)
	      Unit
	    else 
	      boundaries(cPos) match {
	      	case NoBoundary => inner(sPos,cPos+1)
	      	case WBoundaryDrop1 | WBoundaryNoDrop1 => {
	      	  val context = medialContext(cPos)
	      	 toSurface(context.w1U,context.w1O,context.w2U)
 	      	  //_logProbTrack+=math.log(update(context.w1U)*toSurface(context.w1U,context.w1O,context.w2U))
	      	 update(context.w1U)
	      	 data.addTransformation(context.w1O, context.w1U, context.rU)
 	      	  inner(cPos+1,cPos+1)
	      	}
	      	case UBoundaryDrop1 | UBoundaryNoDrop1 => {
	      	  val context = finalContext(cPos)
	      	  //_logProbTrack+=math.log(update(context.wU)*toSurface(context.wU,context.wO,data.UBOUNDARYWORD))
	      	  update(context.wU)
 	      	  data.addTransformation(context.wO, context.wU, data.UBOUNDARYWORD)
	      	  nUtterances+=1
	      	  
 	      	  inner(cPos+1,cPos+1) 	      	  
	      	}
	  }
	  if (gold)
	    data.boundaries=data.goldBoundaries.clone
	  if (binitProb != -1)
	    ungoldType(data.boundaries, 0)
	  if (data.dropProb==0)
	    undrop(data.boundaries, 0)
	  val res = inner(1,1)
	  if (wordseg.DEBUG)
		  assert(pypUni.sanityCheck)
	  res
	}	

	
   def hyperParam: String = "alpha0 "+pypUni.concentration
   
   def resampleConcentration = {
      def logpdf(alpha: Double): Double = {
	      var result = 0
	      val logPrior = Utils.lgammadistShapeRate(alpha,wordseg.wordseg.shape,wordseg.wordseg.rate)
	      pypUni.logProbSeating(alpha)+logPrior
	    }
      val alpha0 = new SliceSampler(logpdf,0.0,Double.PositiveInfinity).sliceSample1D(pypUni.concentration, pypUni.concentration/32.0, 20)      
	  pypUni.concentration = alpha0 
	}	  
	
	def sanity =
	  pypUni.sanityCheck
	
	/**
	 * context utterance medial ==> w1, w2, w1w2, isFinal-information
	 */
	def medialContext(pos: Int) = {
	  assert(pos>0 && boundaries(pos)!=UBoundaryDrop1 && boundaries(pos)!=UBoundaryNoDrop1 && pos<(boundaries.length-1))
	  val w1Start = data.boundaryToLeft(pos-1)
	  val w2End = data.boundaryToRight(pos+1)
	  val (w1O,w1U,w1D) = data.getWordWithVar(w1Start, pos)
	  val (w2O,w2U) = data.getWord(pos, w2End)
	  val w1w2O = concat(w1O,w2O)
	  val w1w2U = concat(w1O,w2U)
	  data.boundaries(w2End) match {
	    case UBoundaryDrop1 | UBoundaryNoDrop1 =>
	      new UnigramMedialContext(w1O,w1U,w1D,w2O,w2U,w1w2O,w1w2U,data.UBOUNDARYWORD,data.UBOUNDARYWORD)
	    case _ =>
	      val rightEnd = data.boundaryToRight(w2End+1)
		  val (rightO,rightU) = data.getWord(w2End,rightEnd)
		  new UnigramMedialContext(w1O,w1U,w1D,w2O,w2U,w1w2O,w1w2U,rightO,rightU)
	  }
	}
	
	/**
	 * context utterance final ==> w1
	 */
	def finalContext(pos: Int) = {
	  assert(pos>0 && (boundaries(pos)==UBoundaryDrop1 || boundaries(pos)==UBoundaryNoDrop1))
	  val w1Start = data.boundaryToLeft(pos-1)
	  val (w1O,w1U,w1D) = data.getWordWithVar(w1Start, pos)
	  new UnigramFinalContext(w1O,w1U,w1D)
	}
	

	def boundaryContext(pos: Int): UContext = boundaries(pos) match {
	  case UBoundaryDrop1 | UBoundaryNoDrop1 => finalContext(pos)
	  case _ => medialContext(pos)
	}
	
	/**
	 * whether or not a drop occured is handled fully by what you pass
	 * dpseg2 ignores the final continuation-probability (cont2)
	 */
	def _boundary(w1Under: WordType,w2Under: WordType,w1Obs: WordType,w2Obs: WordType, rU: WordType, rO: WordType) = {
	  val predw1 = pypUni(w1Under)
	  val predw2 = pypUni(w2Under)//,List(w1Under))
	  val cont1 = (1-_predBoundary())
	  val isFinal = rU==data.UBOUNDARYWORD
	  val cont2 = if (isFinal) _predBoundary(1) else (1-_predBoundary(1))
	  predw1 *  toSurface(w1Under,w1Obs,rU) * cont1 * 
	  predw2 * toSurface(w2Under,w2Obs,rU) * cont2
	}


	/**
	 * dpseg2 ignores the word-final factor (cont), leading to different results
	 */
	def _noBoundary(w1w2Under: WordType, w1w2Obs: WordType, rU: WordType, rO: WordType) = {
	  val predw1w2 = pypUni(w1w2Under)
	  val isFinal = rU==data.UBOUNDARYWORD
	  val cont = if (isFinal) _predBoundary() else (1-_predBoundary())
	  predw1w2 * toSurface(w1w2Under,w1w2Obs,rU) * cont
	}
	
	def _pronProb(w1U: WordType, w1O: WordType,rU: WordType,rO: WordType) = pypUni(w1U)*toSurface(w1U,w1O,rU)
	
	
	/**
	 * returns a distribution over all possible ways to resample
	 * a boundary position
	 */
	def _calcMedialHypotheses(w1O: WordType,w1D: WordType,
					  w2O: WordType,w2U: WordType,
					  w1w2O: WordType, w1w2U: WordType, rU: WordType, rO: WordType): Categorical[Boundary] = {
	  val res: Categorical[Boundary] = new Categorical
	  res.add(NoBoundary,
	      _noBoundary(w1w2U,w1w2O,rU,rO))
	  if (wordseg.wordseg.dropInferenceMode != IGNOREDROP && !wordseg.wordseg.isAnnealing) {
		  res.add(WBoundaryDrop1,
				  _boundary(w1D,w2U,w1O,w2O,rU,rO))
	  }
	  res.add(WBoundaryNoDrop1,
	      _boundary(w1O,w2U,w1O,w2O,rU,rO))
	  assert(res.partition>0)
	  res
	}
	
	def _calcFinalHypotheses(w1O: WordType, w1D: WordType): Categorical[Boundary] = {
	  val res: Categorical[Boundary] = new Categorical
	  if (wordseg.wordseg.dropInferenceMode != IGNOREDROP && !wordseg.wordseg.isAnnealing) {
		  res.add(UBoundaryDrop1,
		      _pronProb(w1D, w1O,data.UBOUNDARYWORD,data.UBOUNDARYWORD))
	  }
	  res.add(UBoundaryNoDrop1,
	      _pronProb(w1O,w1O,data.UBOUNDARYWORD,data.UBOUNDARYWORD))
	  assert(res.partition>0)
	  res
	}
	
	def _calcHypotheses(context: UContext): Categorical[Boundary] = {
		context match {
		  case UnigramFinalContext(w1O,w1U,w1D) => _calcFinalHypotheses(w1O,w1D)
		  case UnigramMedialContext(w1O,w1U,w1D,w2O,w2U,w1w2O,w1w2U,rU,rO) => 
		    _calcMedialHypotheses(w1O,w1D,w2O,w2U,w1w2O,w1w2U,rU,rO)
		}
	}
	
	def updateBoundary(pos: Int, b: Boundary, context: UContext) = {
//	  print("[updated: "+pypUni.logProb+" "+_logProbTrack)	  
	  val hasChanged = boundaries(pos)!=b
	  data.setBoundary(pos, b)
	  context match {
	    case UnigramMedialContext(w1O,w1U,w1D,w2O,w2U,w1w2O,w1w2U,rU,rO) =>
	      	val isFinal = rU==data.UBOUNDARYWORD 
	    	b match {
	    	  case NoBoundary => 
	    	    //_logProbTrack+=math.log(update(w1w2U)*toSurface(w1w2U,w1w2O,rU))
	    	    update(w1w2U)
	    	  case WBoundaryDrop1 => 
	    	    //_logProbTrack+=math.log(update(w1D)*toSurface(w1D,w1O,rU))
	    	    update(w1D)
	    	    data.addDrop1(w1D.get(w1D.size-2),w2O.get(0))
	    	    //_logProbTrack+=math.log(update(w2U)*toSurface(w2U,w2O,rU))
	    	    update(w2U)
	    	  case WBoundaryNoDrop1 =>
//	    	    print(" -->("+w1O+") ")
//	    	    _logProbTrack+=math.log(update(w1O)*toSurface(w1O,w1O,rU))
	    	    update(w1O)
//	    	    print(pypUni.logProb+" "+_logProbTrack+" -->("+w2U+") ")
//	    	    _logProbTrack+=math.log(update(w2U)*toSurface(w2U,w2O,rU))
	    	    update(w2U)
	    	    if (w1O.get(w1O.size()-1)==data.DROPSEG && w1O.size>1)
	    	    	data.addNoDrop1(w1O.get(w1O.size-2),w2O.get(0))	    	
//	    	    print(pypUni.logProb+" "+_logProbTrack)
	    	}
	    	if (isFinal) nUtterances+=1
	    case UnigramFinalContext(w1O,w1U,w1D) =>
	      b match {
	        case UBoundaryDrop1 =>
//	          _logProbTrack+=math.log(update(w1D)*toSurface(w1D,w1O,data.UBOUNDARYWORD))
	          update(w1D)
	          data.addDrop1(w1D.get(w1D.size-2),data.UBOUNDARYWORD.get(0))	          
	        case UBoundaryNoDrop1 =>
//	          _logProbTrack+=math.log(update(w1O)*toSurface(w1O,w1O,data.UBOUNDARYWORD))
	          update(w1O)
	    	  if (w1O.get(w1O.size()-1)==data.DROPSEG && w1O.size>1)      
	    		   data.addNoDrop1(w1O.get(w1O.size-2),data.UBOUNDARYWORD.get(0))	          
	      }
	  }
	  if (wordseg.DEBUG) {
		  assert(pypUni.sanityCheck)
	  }
//      print("]")
	}
	
	def removeAssociatedObservations(context: UContext, boundary: Boundary) = {//hasBoundary: Boolean) =
//	  print("[remove: "+pypUni.logProb +" "+_logProbTrack+" --> ")
	  context match {
	  case UnigramMedialContext(w1O,w1U,w1D,w2O,w2U,w1w2O,w1w2U,rU,rO) =>
	    val isFinal=rU==data.UBOUNDARYWORD
	    if (isFinal) nUtterances-=1
	    if (boundary==WBoundaryDrop1 || boundary==WBoundaryNoDrop1) {
	      data.removeTransformation(w1O, w1U, w2U)        	      
	      //_logProbTrack-=math.log(remove(w1U)*toSurface(w1U,w1O,rU))
	      remove(w1U)
	      remove(w2U)
	      //_logProbTrack-=math.log(remove(w2U)*toSurface(w2U,w2O,rU))	      
	    } else
	    	remove(w1w2U)
	        //_logProbTrack-=math.log(remove(w1w2U)*toSurface(w1w2U,w1w2O,rU))
	  case UnigramFinalContext(w1O,w1U,w1D) =>
	    data.removeTransformation(w1O, w1U, data.UBOUNDARYWORD)
	    //_logProbTrack-=math.log(remove(w1U)*toSurface(w1U,w1O,data.UBOUNDARYWORD))
	    remove(w1U)
	  }
	  if (wordseg.DEBUG) {
		  assert(pypUni.sanityCheck)
	  }
//	  pypUni.logProb
//	  print(pypUni.logProb+" "+_logProbTrack+"]")
	}
	
	def resample(pos: Int, anneal: Double=1.0): Unit = {
//		print(pos + " "+boundaries(pos)+ " "+pypUni._tCount+" -> ")
	    val context = boundaryContext(pos)
		removeAssociatedObservations(context,boundaries(pos))//==WBoundaryDrop || boundaries(pos)==WBoundaryNodrop)
		val result = _calcHypotheses(context)
		if (anneal==1.0)
		  updateBoundary(pos, result.sample,context)
		else
		  updateBoundary(pos, result.sample(anneal),context)
//		println(boundaries(pos)+" "+pypUni._tCount+" "+pypUni.logProb+" "+_logProbTrack)
	}
	
	def gibbsSweep(anneal: Double=1.0): Double = {
	  for (i: Int <- shuffle(1 until boundaries.length)) { 
		  resample(i,anneal)
	  }
	  logProb
	}

  def __reseatProbsFinal(w1D: WordType, w1O: WordType): Categorical[Boundary] = {
	  val res = new Categorical[Boundary]
	  res.add(UBoundaryDrop1, pypUni(w1D)*toSurface(w1D, w1O,data.UBOUNDARYWORD))
	  res.add(UBoundaryNoDrop1, pypUni(w1O)*toSurface(w1O,w1O,data.UBOUNDARYWORD))
	  res
	}
  
  def __reseatProbs(w1D: WordType, w1O: WordType,rU: WordType, rO: WordType): Categorical[Boundary] = {
	  val res = new Categorical[Boundary]
	  res.add(WBoundaryDrop1, pypUni(w1D)*toSurface(w1D, w1O,rU))
	  res.add(WBoundaryNoDrop1, pypUni(w1O)*toSurface(w1O,w1O,rU))
	  res
	}
	
	def resampleWords(pos: Int, anneal: Double) = {
	  boundaries(pos) match {
	    case NoBoundary => Unit
	    case _ =>
		    val context = boundaryContext(pos)
		    removeAssociatedObservations(context, boundaries(pos))
		    context match {
		    	case UnigramMedialContext(w1O,w1U,w1D,w2O,w2U,w1w2O,w1w2U,rU,rO) =>
		    		val res = __reseatProbs(w1D, w1O,rU,rO)
		    		updateBoundary(pos, res.sample(anneal),context)
		    	case UnigramFinalContext(wO,wU,wD) =>
		    	  val res = __reseatProbsFinal(wD,wO)
		    	  updateBoundary(pos, res.sample(anneal),context)
		    }	      
	  }
	}
	
	def gibbsSweepWords(anneal: Double=1.0): Double = {
	  for (i: Int <- 1 until boundaries.length) {
		  resampleWords(i,anneal)
	  }
	  logProb
	}
	
	def removeSentence(sentence:(Int,Int)): (Vector[Boundary],Double) = {
	  var lprob = 0.0
	  var spos=sentence._1
	  var isFinal = false
	  while (!isFinal) {
	    val epos = data.boundaryToRight(spos+1)
	    isFinal = data.boundaries(epos) match {
	      case UBoundaryDrop1 | UBoundaryNoDrop1 => true
	      case _ => false
	    }
	    val (wO,wU) = data.getWord(spos,epos)
	    println(spos+" "+epos+"="+wO)
	    lprob+=math.log(remove(wU))
	    if (!isFinal) {
	      lprob += math.log(1-_predBoundary()) 
	    }
	    spos = epos
	  }
	  nUtterances-=1
	  lprob+=math.log(_predBoundary())
	  (Vector.empty[Boundary].++(data.boundaries.view(sentence._1+1, sentence._2+1)),lprob)
	}
	
	def mhsResample(sentence:(Int,Int)) = {
	  val length = data.getSentenceLength(sentence)
	  val trellis = new Array[(Int,Double)](length)
	  val (oldSeg,oldSegLP) = removeSentence(sentence)
	  //trellis[x] stores the highest probability of any segmentation of the string of length x
	  //and the start-position of its last word as a backpointer
//	  trellis[0] = (-1,pypUni(data.getWordSentence
//	  for (bpos <- 1 to length) {
	    
	  }
	  
	
	
	def logProb: Double = {
	  val nonFinal = nTokens - nUtterances
	  val lpBoundaries = Gamma.logGamma(nonFinal+betaUB/2.0)+Gamma.logGamma(nUtterances+betaUB/2.0)+Gamma.logGamma(betaUB)-
	  						2*Gamma.logGamma(betaUB/2.0)-Gamma.logGamma(nTokens+betaUB)
	  if (npbayes.wordseg.DEBUG)
		  assert(pypUni.sanityCheck)
	  pypUni.logProb + data.delModelProb + {
	    if (wordseg.wordseg.hyperparam)
	      Utils.lgammadistShapeRate(pypUni.concentration,wordseg.wordseg.shape,wordseg.wordseg.rate)
	    else
	      0
	  } + lpBoundaries
	} 

}