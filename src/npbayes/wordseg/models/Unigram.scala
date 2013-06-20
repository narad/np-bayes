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
import npbayes.Utils
import java.util.Random
import npbayes.wordseg.LexGenerator
import npbayes.wordseg.UNIUNLEARNED
import npbayes.wordseg.UNILEARNED
import npbayes.wordseg.UNILEARNEDVOWELS
import org.apache.commons.math3.special.Gamma
import optimizer.samplers1D
import npbayes.WordType
import org.apache.commons.math3.analysis.UnivariateFunction
import org.apache.commons.math3.optimization.univariate.BrentOptimizer
import breeze.optimize.LBFGS
import breeze.linalg.DenseVector




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
					  

class Unigram(val corpusName: String, val features: (Int,((WordType,WordType))=>Array[Double]),concentration: Double,discount: Double=0,val pWB: Double = 0.5, val assumption: HEURISTIC = EXACT,val dropSeg: String = "KLRK", val dropInd: String = "KLRK",
    val lexgen: LexGenerator, val phonVar: Boolean=false) extends WordsegModel {
	require(0<=discount && discount<1)
	require(if (discount==0) concentration>0 else concentration>=0)
	val betaUB = 2.0 
	val data = new Data(corpusName,phonVar,dropInd,dropSeg,"0","0",features)
	//nSymbols-2 because of the "$" and the drop-indicator symbol
	//val pypUni = new CRP[WordType](concentration,discount,new MonkeyUnigram(SymbolTable.nSymbols-2,0.5),assumption)
	val pypUni = {
	  val tlexgen = lexgen match {
	    case UNIUNLEARNED =>
	    	new MonkeyUnigram(npbayes.wordseg.data.SymbolSegTable.nSymbols-2,0.5)
	    case UNILEARNED =>
	       new UnigramLearned(npbayes.wordseg.data.SymbolSegTable.nSymbols-2,1.0,false)
	    case UNILEARNEDVOWELS =>
	       new UnigramLearned(npbayes.wordseg.data.SymbolSegTable.nSymbols-2,1.0,true)
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
	
	
//	def evaluate =
//	  data.evaluate.toString
	
	def boundaries = data.boundaries
//	def rules = data.rules
	def nTokens = pypUni._oCount

	def update(w: WordType) = pypUni.update(w)

	def remove(w: WordType)= pypUni.remove(w)

	/**
	 * make sure distortion only happens when we actually use a model with drops
	 */
	def toSurface(u: WordType,s: WordType, r:WordType): Double = {
	  if (phonVar==false) {
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
	 * initializes the data and the model
	 */
	override def init(gold:Boolean = false, binitProb:Double = 0.5) = { 
	  def inner(bPos: Int): Unit = 
	    if (bPos>=data.nBoundaries)
	      Unit
	    else 
	      boundaries(bPos) match {
	      	case false => inner(bPos+1)
	      	case true =>
	      	  update(data.getUnderlyingWordAt(bPos))
	      	  if (data.uboundaries(bPos)) 
		      	  nUtterances+=1	      	  
 	      	  inner(bPos+1)	      	    
	      	  }
	  if (gold) {
	    data.boundaries.clear
	    data.boundaries++=data.goldboundaries
	    data.buildWords
	  } else {
	    data.boundaries.clear()
	    data.boundaries++=data.randomBoundaries(binitProb)
	    data.buildWords
	  }
	  val res = inner(0)
	  if (phonVar) data.updateDel1Model
	  if (wordseg.DEBUG)
		  assert(pypUni.sanityCheck)
	}	

	
   override def hyperParam: String = "alpha0 "+pypUni.concentration
   

   override def uniCustomers = pypUni._oCount
   
   override def resampleConcentration(hsiters: Int = 1) = {
      def proposallogpdf(x: Double,y: Double): Double = {
        math.log(gaussian(y,math.abs(y)*wordseg.wordseg.hsmhvar,x))
      }
 
      def proposalsample(y: Double): Double = {
        nextGaussian(y, math.abs(y)*wordseg.wordseg.hsmhvar)
      }
      
      def logpdf(alpha: Double): Double = {
	      var result = 0
	      val logPrior = Utils.lgammadistShapeScale(alpha,wordseg.wordseg.shape,wordseg.wordseg.rate)
	      //pypUni.propLogProb(alpha)+logPrior
	      pypUni._logProbSeatingByConc(alpha)+logPrior
	    }
      val alpha0 = wordseg.wordseg.hsample match {
        case "slice" | "sliceadd" | "slicecheck" =>
            def samplefunc(x0p: Double,oldlp: Double) = wordseg.wordseg.hsample match {
	          case "slice" => samplers1D.slicesampleDouble(x0p, logpdf,oldlp)
	          case "sliceadd" => samplers1D.slicesampleAdd(x0p, logpdf,oldlp)
	          case "slicecheck" => samplers1D.slicesampleCheck(x0p, logpdf,oldlp)
	        } 
        	var tmpx0 = pypUni.concentration
        	var oldlogpdf:Double = 1.0
	         for (i <- 0 until hsiters) {
	           val tmp = samplefunc(tmpx0,oldlogpdf)
	           tmpx0 = tmp._1
	           oldlogpdf = tmp._2
	         }
	         tmpx0	             
        case "mh" =>
          samplers1D.mhsample(pypUni.concentration, logpdf, proposallogpdf, proposalsample, hsiters, true)
      }
	  pypUni.concentration = alpha0 
	}	  
	
   override def optimizeConcentration = {
      def logpdf(alpha: Double): Double = {
	      var result = 0
	      val logPrior = Utils.lgammadistShapeScale(alpha,wordseg.wordseg.shape,wordseg.wordseg.rate)
	      pypUni.propLogProb(alpha)+logPrior
	    }
      def logpdfV(alpha: DenseVector[Double]): Double = logpdf(alpha.data(0))
      val lbfgs = new breeze.optimize.LBFGS[DenseVector[Double]](1000,3)
      
      val concentrationUni = optimizer.approxGradientDescent1D(pypUni.concentration,logpdf)
//      val concentrationUni = lbfgs.minimize(new breeze.optimize.ApproximateGradientFunction(logpdfV), DenseVector.fill(1)(pypUni.concentration)).data(0)      
      pypUni.concentration = concentrationUni
      wordseg.wordseg.hyperSampleFile.print(concentrationUni-1 + " " + logpdf(concentrationUni-1)+"\n")
      wordseg.wordseg.hyperSampleFile.print(concentrationUni+" "+logpdf(concentrationUni)+" <-\n")	      
      wordseg.wordseg.hyperSampleFile.print(concentrationUni+1+" "+logpdf(concentrationUni+1)+"\n\n")      
	}	  

   
   override def sanity =
	  pypUni.sanityCheck
	
	
	/**
	 * whether or not a drop occured is handled fully by what you pass
	 * dpseg2 ignores the final continuation-probability (cont2)
	 */
	def _boundary(w1Under: WordType,w2Under: WordType,w1Obs: WordType,w2Obs: WordType, rU: WordType) = {
	  val predw1 = pypUni(w1Under)
	  val predw2 = pypUni(w2Under)//,List(w1Under))
	  val cont1 = (1-_predBoundary())
	  val isFinal = rU==data.UBOUNDARYWORD
	  val cont2 = if (isFinal) _predBoundary(1) else (1-_predBoundary(1))
	  predw1 *  toSurface(w1Under,w1Obs,w2Under) * cont1 * 
	  predw2 * toSurface(w2Under,w2Obs,rU) * cont2
	}


	/**
	 * dpseg2 ignores the word-final factor (cont), leading to different results
	 */
	def _noBoundary(w1w2Under: WordType, w1w2Obs: WordType, rU: WordType) = {
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
	def _calcMedialHypotheses(c: MedialContext): Categorical[(Boundary,Rule)] = {
	  val res: Categorical[(Boundary,Rule)] = new Categorical
	  res.add((NoBoundary,NoRule),
	      _noBoundary(c.w12U,c.w12O,c.rightU))
	  if (wordseg.wordseg.dropInferenceMode != IGNOREDROP && !wordseg.wordseg.isAnnealing) {
		  res.add((WBoundary,Del1),
				  _boundary(c.w1D1,c.w2U,c.w1O,c.w2O,c.rightU))
		  res.add((WBoundary,Del2),
				  _boundary(c.w1D2,c.w2U,c.w1O,c.w2O,c.rightU))		
	  }
	  res.add((WBoundary,NoRule),
			  _boundary(c.w1O,c.w2U,c.w1O,c.w2O,c.rightU))
	  assert(res.partition>0)
	  res
	}
	
	def _calcFinalHypotheses(c: FinalContext): Categorical[(Boundary,Rule)] = {
	  val res: Categorical[(Boundary,Rule)] = new Categorical
	  if (wordseg.wordseg.dropInferenceMode != IGNOREDROP && !wordseg.wordseg.isAnnealing) {
		  res.add((UBoundary,Del1),
		      _pronProb(c.w1D1, c.w1O,data.UBOUNDARYWORD,data.UBOUNDARYWORD))
		  res.add((UBoundary,Del2),
		      _pronProb(c.w1D2, c.w1O,data.UBOUNDARYWORD,data.UBOUNDARYWORD))		      
	  }
	  res.add((UBoundary,NoRule),
			  _pronProb(c.w1O,c.w1O,data.UBOUNDARYWORD,data.UBOUNDARYWORD))
	  assert(res.partition>0)
	  res
	}
	
	def _calcHypotheses(context: AContext): Categorical[(Boundary,Rule)] = 
	  context match {
	  	case c: FinalContext => _calcFinalHypotheses(c)
	  	case c: MedialContext => _calcMedialHypotheses(c)
	  }
	  
	
	
	def updateBoundary(pos: Int, br:(Boundary,Rule), context: AContext) = {
	  val (b,r) = br
	  context match {
	    case MedialContext(_,w1U,w1O,w1D1,w1D2,w2U,w2O,w12U,w12O,rU,w1Pos) =>
	      	val isFinal = rU==data.UBOUNDARYWORD 
	    	b match {
	    	  case NoBoundary => 
	    	    update(w12U)
	    	    data.insertWord(w1Pos, (w12U,w12O))
	    	  case WBoundary =>
	    	    data.boundaries+=pos
	    	    r match {
	    	      case Del1 =>
	    	        update(w1D1)
	    	        data.insertWord(w1Pos, (w1D1,w1O))
	    	      case Del2 =>
	    	        update(w1D2)
	    	        data.insertWord(w1Pos, (w1D2,w1O))
	    	      case NoRule =>
	    	        update(w1O)
	    	        data.insertWord(w1Pos, (w1O,w1O))
	    	    }
    	        update(w2U)
    	        data.insertWord(pos,(w2U,w2O))
	    	}
	    	if (isFinal) nUtterances+=1
	    case FinalContext(_,w1U,w1O,w1D1,w1D2,w1Pos) =>
	      r match {
	        case Del1 =>
	          update(w1D1)
	          data.insertWord(w1Pos, (w1D1,w1O))
	        case Del2 =>
	          update(w1D2)
	          data.insertWord(w1Pos, (w1D2,w1O))	              
	        case NoRule =>
	          update(w1O)
	          data.insertWord(w1Pos, (w1O,w1O))
	      }
	  }
//	  println("Insert a"+r+" unigram restaurant: "+pypUni.labelTabels)	  
	  if (wordseg.DEBUG) {
		  assert(pypUni.sanityCheck)
	  }
	}
	
	def removeAssociatedObservations(context: AContext, boundary: Boolean) = {//hasBoundary: Boolean) =
	  context match {
	  case MedialContext(_,w1U,w1O,w1D1,w1D2,w2U,w2O,w1w2U,w1w2O,rU,w1Pos) =>
	    val isFinal=rU==data.UBOUNDARYWORD
	    if (isFinal) nUtterances-=1
	    if (boundary) {        	      
	      remove(w1U)
	      remove(w2U)
	    } else
	    	remove(w1w2U)
	  case FinalContext(_,w1U,w1O,_,_,_) =>
	    remove(w1U)
	  }
	  if (wordseg.DEBUG) {
		  assert(pypUni.sanityCheck)
	  }
	}
	
	def resample(pos: Int, anneal: Double=1.0): Unit = {
	    val boundary = boundaries(pos)
	    val context = data.removeBoundary(pos)
		removeAssociatedObservations(context,boundary)
		val result = _calcHypotheses(context)
		if (anneal==1.0)
		  updateBoundary(pos, result.sample,context)
		else
		  updateBoundary(pos, result.sample(anneal),context)
	}
	
	override def gibbsSweep(anneal: Double=1.0): Double = {
	  for (i: Int <- 1 to data.nBoundaries) {
//		  println("Resample "+i)
		  resample(i,anneal)
	  }
	  if (phonVar) data.updateDel1Model
	  logProb
	}

  def __reseatProbsFinal(w1D1: WordType, w1D2: WordType, w1O: WordType): Categorical[(Boundary,Rule)] = {
	  val res = new Categorical[(Boundary,Rule)]
	  res.add((UBoundary,Del1), pypUni(w1D1)*toSurface(w1D1, w1O,data.UBOUNDARYWORD))
//	  res.add((UBoundary,Del2), pypUni(w1D2)*toSurface(w1D2, w1O,data.UBOUNDARYWORD))
	  res.add((UBoundary,NoRule), pypUni(w1O)*toSurface(w1O,w1O,data.UBOUNDARYWORD))
	  res
	}
  
  def __reseatProbs(w1D1: WordType, w1D2: WordType, w1O: WordType,rU: WordType): Categorical[(Boundary,Rule)] = {
	  val res = new Categorical[(Boundary,Rule)]
	  res.add((WBoundary,Del1), pypUni(w1D1)*toSurface(w1D1, w1O,rU))
//	  res.add((WBoundary,Del2), pypUni(w1D2)*toSurface(w1D2, w1O,rU))	  
	  res.add((WBoundary,NoRule), pypUni(w1O)*toSurface(w1O,w1O,rU))
	  res
	}
	
	def resampleWords(pos: Int, anneal: Double) = {
	  boundaries(pos) match {
	    case false => Unit
	    case true =>
	      	val boundary = boundaries(pos)
		    val context = data.removeBoundary(pos)
		    removeAssociatedObservations(context, boundary)
		    context match {
		    	case MedialContext(_,w1U,w1O,w1D1,w1D2,w2U,w2O,w1w2U,w1w2O,rU,w1Pos) =>
		    		val res = __reseatProbs(w1D1, w1D2, w1O,rU)
		    		updateBoundary(pos, res.sample(anneal),context)
		    	case FinalContext(_,wU,wO,wD1,wD2,wPos) =>
		    	  val res = __reseatProbsFinal(wD1,wD2,wO)
		    	  updateBoundary(pos, res.sample(anneal),context)
		    }	      
	  }
	}
	
	override def gibbsSweepWords(anneal: Double=1.0): Double = {
//	  println("deletionpoints: "+data.del1s+" words: "+data.words)
	  for (i: Int <- shuffle(1 to data.uboundaries.last)) {
		  resampleWords(i,anneal)
	  }
	  if (phonVar) 
	    wordseg.wordseg.loglearn match {
	      case "sample" => data.updateDel1ModelSample
	      case "optimize" => data.updateDel1Model
	    }
	  logProb
	}
	
	   
	
	override def logProb: Double = {
	  val nonFinal = nTokens - nUtterances
	  val lpBoundaries = Gamma.logGamma(nonFinal+betaUB/2.0)+Gamma.logGamma(nUtterances+betaUB/2.0)+Gamma.logGamma(betaUB)-
	  						2*Gamma.logGamma(betaUB/2.0)-Gamma.logGamma(nTokens+betaUB)
	  val lp1 = pypUni.logProb
	  val lp2 = if (phonVar) data.delModelProb else 0
	  val lp3 = if (wordseg.wordseg.hyperparam!="no")
	      Utils.lgammadistShapeScale(pypUni.concentration,wordseg.wordseg.shape,wordseg.wordseg.rate)
	    else
	      0 
	  if (npbayes.wordseg.DEBUG)
		  assert(pypUni.sanityCheck)
	  System.err.println("lp1: "+ lp1 + " lp2: "+ lp2+ " lp3: "+lp3+" boundaries: "+ lpBoundaries)		  
	  lp1 + lp2 + lp3+lpBoundaries
	} 
	
	def uniTables: Int = pypUni._tCount

}