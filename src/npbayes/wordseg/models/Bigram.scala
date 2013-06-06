package npbayes.wordseg.models

import npbayes.wordseg
import npbayes.WordType
import npbayes.distributions._
import npbayes.wordseg.data._
import npbayes.wordseg.lexgens._
import scala.util.Random.shuffle
import java.util.HashMap
import java.io.PrintStream
import npbayes.wordseg.IGNOREDROP
import npbayes.Utils
import org.apache.commons.math3.random.RandomGenerator
import java.util.Random
import npbayes.wordseg.LexGenerator
import npbayes.wordseg.BIUNLEARNED
import npbayes.wordseg.BILEARNEDVOWELS
import npbayes.wordseg.BILEARNED
import optimizer.samplers1D
import scala.collection.mutable.OpenHashMap
import org.apache.commons.math3.analysis.UnivariateFunction
import org.apache.commons.math3.optimization.univariate.BrentOptimizer
abstract class BContext

case class BigramMedialContext(val leftU: WordType, val w1O: WordType, val w1U: WordType, val w1D: WordType,
					  val w2O: WordType, val w2U: WordType,
					  val w1w2O: WordType, val w1w2U: WordType,
					  val rightO: WordType, val rightU: WordType) extends BContext 

case class BigramFinalContext(val leftU: WordType, val wO: WordType, val wU: WordType, val wD: WordType) extends BContext					  

object Bigram {
  val FAITHFUL = false
}

class Bigram(val corpusName: String,var concentrationUni: Double,discountUni: Double=0,var concentrationBi: Double, var discountBi: Double=0,val pStop: Double = 0.5, val assumption: HEURISTIC = EXACT,
    		  val dropSeg: String = "KLRK", val dropInd: String = "KLRK",val dropProb: Double = 0.0,
    		  val lexgen: LexGenerator) extends WordsegModel {
	require(0<=discountUni && discountUni<1)
	require(if (discountUni==0) concentrationUni>0 else concentrationUni>=0)
	
	val unif= new Random
	val data = new Data(corpusName,dropProb,dropInd,dropSeg,"","")
	val pypUni = { 
		val tlexgen = lexgen match {
		  case BIUNLEARNED =>
	    	    new MonkeyBigram(npbayes.wordseg.data.SymbolSegTable.nSymbols-2,0.5,data.UBOUNDARYWORD,0.5)
		  case BILEARNEDVOWELS =>
	    	    new BigramLearned(npbayes.wordseg.data.SymbolSegTable.nSymbols-2,data.UBOUNDARYWORD,0.5,1.0,true)		    
		  case BILEARNED =>
	          new BigramLearned(npbayes.wordseg.data.SymbolSegTable.nSymbols-2,data.UBOUNDARYWORD,0.5,1.0,false)
		  case _ =>
		    throw new Error("Invalid value for lexgen:"+lexgen)
	      }
		new CRP[WordType](concentrationUni,discountUni,tlexgen,assumption)
	}
	val pypBis: HashMap[WordType,CRP[WordType]] = new HashMap	
 

	
	def boundaries = data.boundaries
	def nTokens = pypUni._oCount
	
/*	def evaluate =
	  data.evaluate.toString

	def update(precedingW: WordType, word: WordType): Double = {
	  val tRest = pypBis.get(precedingW)
	  if (tRest==null) {
	    val newRest = new CRP[WordType](concentrationBi,discountBi,pypUni,assumption)
	    val res = newRest.update(word)
	    pypBis.put(precedingW, newRest)
	    res
	  } else {
	    tRest.update(word)
	  }
	}

	def removeWrap(precedingW: WordType, word: WordType): Double = {
	  val tRest = pypBis.get(precedingW)
	  val res = tRest.remove(word)
	  if (tRest.isEmpty) {
	    pypBis.remove(precedingW)
	  }
	  res
	}
	
	def sanity: Boolean = {
	  val resIter = pypBis.values.iterator
	  val res = 0.0
	  while (resIter.hasNext) {
	    val pypW = resIter.next()
	    assert(pypW.sanityCheck)	    
	  }
	  assert(pypUni.sanityCheck)
	  true
/*	 (for (pypW <- pypBis.values.toList) //TODO convert check counts
	    yield {
		assert(pypW.sanityCheck)
	    pypW.sanityCheck}).reduce(_&&_)&&{assert(pypUni.sanityCheck); 
	    pypUni.sanityCheck} &&
	    pypUni._tCount == pypUni.base.asInstanceOf[MonkeyBigram]._nWords + pypUni.base.asInstanceOf[MonkeyBigram]._nUBS
    	pypUni._oCount == {for (w <- pypBis.values.toList) yield w._tCount}.sum*/
	 }

	def toSurface(u: WordType, o:WordType,rU: WordType): Double = {
	  if (wordseg.wordseg.dropInferenceMode==IGNOREDROP) {
	    if (u==o)
	      1.0
	    else
	      0.0
	  } else
		  data.R(u,o,rU)
	}
	def DROPSYMBOL = data.DROP1
	def _phoneSeq = data.data
	
	
	
	def medialContext(pos: Int): BigramMedialContext = {
	  assert(pos>0 && boundaries(pos)!=UBoundary && pos<(boundaries.length-1))
	  val leftWordStart = data.boundaryToLeft(pos-1)
	  val rightWordEnd = data.boundaryToRight(pos+1) 
	  val (w1O,w1U,w1D) = data.getWordWithVar(leftWordStart, pos)
	  val (w2O,w2U) = data.getWord(pos,rightWordEnd)
	  val (w1w2O,w1w2U) = data.getWord(leftWordStart, rightWordEnd)
	  val lU = boundaries(leftWordStart) match {
	    case UBoundary => data.UBOUNDARYWORD
	    case _ => data.getWord(data.boundaryToLeft(leftWordStart-1), leftWordStart)._2
	  }
	  val (rO,rU) = boundaries(rightWordEnd) match {
	    case UBoundary => (data.UBOUNDARYWORD,data.UBOUNDARYWORD)
	    case _ => data.getWord(rightWordEnd, data.boundaryToRight(rightWordEnd+1))
	  }
	  new BigramMedialContext(lU,w1O,w1U,w1D,w2O,w2U,w1w2O,w1w2U,rO,rU)
	}
	
	def finalContext(pos: Int): BigramFinalContext = {
	  assert(pos>0 && boundaries(pos)==UBoundary)
	  val leftWordStart = data.boundaryToLeft(pos-1)
	  val (wO,wU,wD) = data.getWordWithVar(leftWordStart, pos)
	  val lU = boundaries(leftWordStart) match {
	    case UBoundary => data.UBOUNDARYWORD
	    case _ => data.getWord(data.boundaryToLeft(leftWordStart-1), leftWordStart)._2
	  }
	  new BigramFinalContext(lU,wO,wU,wD)
	}
	
	def boundaryContext(pos: Int): BContext = boundaries(pos) match {
	  case UBoundary => finalContext(pos)
	  case _ => medialContext(pos)
	}	
	/**
	 * initializes the CRP with the counts
	 */
	def init(goldBoundaries:Boolean = false, binitProb:Double = 0.5) = {
	  def inner(sPos: Int,cPos: Int): Unit = 
	    if (cPos>=boundaries.length)
	      Unit
	    else 
	      boundaries(cPos) match {
	      	case NoBoundary => inner(sPos,cPos+1)
	      	case WBoundary => {
	      	  val context = medialContext(cPos)
 	      	  _logProbTrack += math.log(update(context.leftU,context.w1U)*toSurface(context.w1U,context.w1O,context.rightU))
 	      	  data.addTransformation(data.whichTransform(context.w1O,context.w1U), context.w1U, context.w2U)
 	      	  inner(cPos+1,cPos+1)
	      	}
	      	case UBoundary => {
	      	  val context = finalContext(cPos)
	      	  _logProbTrack += math.log(update(context.leftU,context.wU)*toSurface(context.wU,context.wO,data.UBOUNDARYWORD))
	      	  _logProbTrack += math.log(update(context.wU,data.UBOUNDARYWORD))
	      	  data.addTransformation(data.whichTransform(context.wO,context.wU), context.wU, data.UBOUNDARYWORD)
	      	  inner(cPos+1,cPos+1)
	      	}
	  }
	  if (goldBoundaries) {
	    data.boundaries=data.goldBoundaries.clone
	    data.rules=data.goldRules.clone
	    for (i <- 1 until data.rules.length) {
	      if (data.rules(i)==Del1)
	        data.rules(i)=NoRule
	    }
	  }
	  inner(1,1)
	}	
	
	
	def predictive(word: WordType, w2: WordType): Double = {
	  val tmpRes = pypBis.get(word)
	  if (tmpRes==null)
	    pypUni.predProb(w2)
	  else
	    tmpRes.predProb(w2)
	}
	
	/**
	 * performs the intermediate updates when calculating probabilities (dpseg2 doesn't do that)
	 * if FAITHFUL == true
	 * 
	 */
	def _noBoundary(left: WordType,w1w2Under: WordType, w1w2Obs: WordType, right: WordType) = {
	  var res = predictive(left,w1w2Under)*toSurface(w1w2Under,w1w2Obs,right)
	  if (Bigram.FAITHFUL)
		  update(left,w1w2Under)
	  res = res * predictive(w1w2Under,right)
	  if (Bigram.FAITHFUL)
		  removeWrap(left,w1w2Under)
	  res
	}
	
	    
	/**
	 * performs the intermediate updates when calculating probabilities (dpseg2 doesn't do that)
	 * if FAITHFUL == true 
	 * whether or not a drop occured is handled fully by what you pass
	 */
	def _boundary(left: WordType, w1Under: WordType,w2Under: WordType,w1Obs: WordType,w2Obs: WordType, right: WordType) = {
	  var res = predictive(left,w1Under)*toSurface(w1Under,w1Obs,w2Under)
	  if (Bigram.FAITHFUL)
		  update(left,w1Under)
	  res = res * predictive(w1Under,w2Under) * toSurface(w2Under,w2Obs,right)
	  if (Bigram.FAITHFUL)
		  update(w1Under,w2Under)
	  res = res * predictive(w2Under,right)
	  if (Bigram.FAITHFUL) {
		  removeWrap(left, w1Under)
		  removeWrap(w1Under,w2Under)
	  }
	  res
	}
	
	  
	def _ubProb(left: WordType, wU: WordType, wO: WordType) =
	  predictive(left,wU)*
	  toSurface(wU,wO,data.UBOUNDARYWORD)*
	  predictive(wU,data.UBOUNDARYWORD)
	
	/**
	 * returns a distribution over all possible ways to resample
	 * an utterance medial boundary position
	 */
	def _calcMedialHypotheses(context: BigramMedialContext): Categorical[(Boundary,Rule)] = {
	  val res: Categorical[(Boundary,Rule)] = new Categorical
	  res.add((NoBoundary,NoRule),
	      _noBoundary(context.leftU,context.w1w2U,context.w1w2O,context.rightU))	 
	  val tRule = data.whichTransform(context.w1O, context.w1U)
	  if (wordseg.wordseg.dropInferenceMode!=IGNOREDROP && !wordseg.wordseg.isAnnealing)    
		  res.add((WBoundary,Del1),
		      _boundary(context.leftU,context.w1D,context.w2U,context.w1O,context.w2O,context.rightU))	    
	  res.add((WBoundary,tRule),
	      _boundary(context.leftU,context.w1O,context.w2U,context.w1O,context.w2O,context.rightU))
	  assert(res.partition>0)
	  res
	}
	
	def _calcFinalHypotheses(context: BigramFinalContext): Categorical[(Boundary,Rule)] = {
	  val res: Categorical[(Boundary,Rule)] = new Categorical
	  if (wordseg.wordseg.dropInferenceMode!=IGNOREDROP && !wordseg.wordseg.isAnnealing)
		  res.add((UBoundary,Del1),
		      _ubProb(context.leftU,context.wD,context.wO))
	  val tRule = data.whichTransform(context.wO,context.wU)
	  res.add((UBoundary,tRule),
	      _ubProb(context.leftU,context.wO,context.wO))
	  assert(res.partition>0)
	  res
	}
	
	def updateBoundary(pos: Int, br: (Boundary,Rule), context: BContext) = {
  	  val (b,r) = br
	  val hasChanged = boundaries(pos)!=b
	  data.setBoundary(pos, b)
	  data.setRule(pos,r)
	  context match {
	    case BigramMedialContext(leftU,w1O,w1U,w1D,w2O,w2U,w1w2O,w1w2U,rightO,rightU) =>
	      b match {
	        case WBoundary =>
	          r match {
	            case Del1 =>
		          _logProbTrack += math.log(update(leftU,w1D)*toSurface(w1D,w1O,w2U))
		          data.addTransformation(r, w1D, w2U)		          
		          _logProbTrack += math.log(update(w1D,w2U)*toSurface(w2U,w2O,rightU))
		          _logProbTrack += math.log(update(w2U,rightU)) //*toSurface(rightU,rightO,null))
	            case NoRule | Rel1 =>
	              _logProbTrack += math.log(update(leftU,w1O)*toSurface(w1O,w1O,w2U))
	              data.addTransformation(r, w1O, w2U)	              
	              _logProbTrack += math.log(update(w1O,w2U)*toSurface(w2U,w2O,rightU))
	              _logProbTrack += math.log(update(w2U,rightU)) //*toSurface(rightU,rightO,null))
	          }  
	        case NoBoundary =>
	          _logProbTrack += math.log(update(leftU,w1w2U)*toSurface(w1w2U,w1w2O,rightU))
	          _logProbTrack += math.log(update(w1w2U,rightU))//*toSurface(rightU,rightO,null))
	      }
	    case BigramFinalContext(leftU,wO,wU,wD) =>
	          r match {
	            case Del1 =>
	            	_logProbTrack += math.log(update(leftU,wD)*toSurface(wD,wO,data.UBOUNDARYWORD))
	            	data.addTransformation(r,wD,data.UBOUNDARYWORD)	            	
	            	_logProbTrack += math.log(update(wD,data.UBOUNDARYWORD))
	            case Rel1 | NoRule =>
	            	_logProbTrack += math.log(update(leftU,wO)*toSurface(wO,wO,data.UBOUNDARYWORD))
	            	data.addTransformation(r,wO,data.UBOUNDARYWORD)
	          		_logProbTrack += math.log(update(wO,data.UBOUNDARYWORD))	              
	            
	          }
	  }
	}
	
	def removeAssociatedObservations(context: BContext, boundary: Boundary, rule: Rule) = {context match {
	  case BigramMedialContext(lU,w1O,w1U,w1D,w2O,w2U,w1w2O,w1w2U,rO,rU) =>
	    boundary match {
	      case WBoundary =>
	        data.removeTransformation(rule, w1U, w2U)
	        _logProbTrack -= math.log(removeWrap(lU, w1U)*toSurface(w1U,w1O,w2U))
	        _logProbTrack -= math.log(removeWrap(w1U,w2U)*toSurface(w2U,w2O,rU))
	        _logProbTrack -= math.log(removeWrap(w2U,rU)) //*toSurface(rU,rO,null))
	      case NoBoundary =>
	        _logProbTrack -= math.log(removeWrap(lU,w1w2U)*toSurface(w1w2U,w1w2O,rU))
	        _logProbTrack -= math.log(removeWrap(w1w2U,rU)) //*toSurface(rU,rO,null))
	    }
	  case BigramFinalContext(lU,wO,wU,wD) =>
	  	data.removeTransformation(rule,wU, data.UBOUNDARYWORD)
	    _logProbTrack -= math.log(removeWrap(lU,wU)*toSurface(wU,wO,data.UBOUNDARYWORD))
	    _logProbTrack -= math.log(removeWrap(wU,data.UBOUNDARYWORD))
	}
	}
	
	def _calcHypotheses(context: BContext): Categorical[(Boundary,Rule)] = context match {
	  case c: BigramMedialContext => _calcMedialHypotheses(c)
	  case c: BigramFinalContext => _calcFinalHypotheses(c)
	}
	
	def resample(pos: Int, anneal: Double=1.0): Unit = {
	  val context = boundaryContext(pos)
	  removeAssociatedObservations(context, boundaries(pos),rules(pos))
	  val result = _calcHypotheses(context)
	  val (newBound,newRule) = {
		  if (anneal==1.0)
			result.sample
		  else
			result.sample(anneal)
		  }
	  updateBoundary(pos, (newBound,newRule),context)
	}
	
	def resampleConcentration(hsiters: Int = 1) = {
	
	  //MH helpers
	  def proposallogpdf(x: Double,y: Double): Double = {
        math.log(gaussian(y,math.abs(y)*wordseg.wordseg.hsmhvar,x))
      }
 
      def proposalsample(y: Double): Double = {
        nextGaussian(y, math.abs(y)*wordseg.wordseg.hsmhvar)
      }
	  
      def resampleUni = {
        def logpdfUni(alpha: Double): Double = {
    	  if (alpha<0)
    		return Double.NegativeInfinity
	      var result = 0
	      val logPrior = 
	        	if (wordseg.wordseg.shape != -1)
		          Utils.lgammadistShapeRate(alpha,wordseg.wordseg.shape,wordseg.wordseg.rate)
		        else
		          0
	      pypUni.logProbSeating(alpha)+logPrior
	    }
        pypUni.concentration = wordseg.wordseg.hsample match {
        case "slice" | "sliceadd" | "slicecheck" => {
         var tmpx0 = pypUni.concentration
         var oldllh = 1.0
         def samplefunc(x0p: Double, oldlp: Double) = wordseg.wordseg.hsample match {
           	case "slice" => samplers1D.slicesampleDouble(x0p, logpdfUni, oldlp)//,tmpx0/32.0)
           	case "sliceadd" => samplers1D.slicesampleAdd(x0p, logpdfUni, oldlp)//,tmpx0/32.0)
           	case "slicecheck" => samplers1D.slicesampleCheck(x0p, logpdfUni, oldlp)//,tmpx0/32.0)
           	case _ => throw new Error("Invalid slice-sampler")
         }
         for (i <- 0 until hsiters) {
           val tmp= samplefunc(tmpx0,oldllh)
           tmpx0 = tmp._1
           oldllh = tmp._2
         }
/*         for (i <- -100 to 200 by 20) {
        	 if (i==0)
        	   wordseg.wordseg.hyperSampleFile.print(tmpx0+i+" "+logpdfUni(tmpx0+i)+" <--\n")
        	 else 
        	   wordseg.wordseg.hyperSampleFile.print(tmpx0+i+" "+logpdfUni(tmpx0+i)+"\n")           
           }
         wordseg.wordseg.hyperSampleFile.print("\n")*/
         tmpx0
        }
        case "mh" =>
         samplers1D.mhsample(pypUni.concentration, logpdfUni, proposallogpdf, proposalsample, wordseg.wordseg.hsampleiters, wordseg.DEBUG)  
      }
      }
            
	  def resampleBiCoupled = {
	      def logpdfBi(alpha: Double): Double = {
	    	  if (alpha<0)
	            return Double.NegativeInfinity
		      var result = 0
		      val logPrior = 
		        if (wordseg.wordseg.shape != -1)
		          Utils.lgammadistShapeRate(alpha,wordseg.wordseg.shape,wordseg.wordseg.rate)
		        else
		          0
		      var res: Double = 0
		      val bCRPit = pypBis.values.iterator
		      while (bCRPit.hasNext) {
		        res += bCRPit.next().logProbSeating(alpha)
		      }
		      assert(res!=Double.NegativeInfinity)
		      res + logPrior
		  } 
	
	      val oldSharedBigramAlpha = concentrationBi 
	      val newSharedBigramAlpha = wordseg.wordseg.hsample match {
	        case "mh" =>
	          samplers1D.mhsample(oldSharedBigramAlpha, logpdfBi, proposallogpdf, proposalsample, wordseg.wordseg.hsampleiters, wordseg.DEBUG)
	        case "slice" | "sliceadd" | "slicecheck" =>
			 var tmpx0 = oldSharedBigramAlpha
			 var oldllh = 1.0
			 def samplefunc(x0p: Double, oldlp: Double) = wordseg.wordseg.hsample match {
           		case "slice" => samplers1D.slicesampleDouble(x0p, logpdfBi, oldlp)//,tmpx0/32.0)
           		case "sliceadd" => samplers1D.slicesampleAdd(x0p, logpdfBi, oldlp)//,tmpx0/32.0)
           		case "slicecheck" => samplers1D.slicesampleCheck(x0p, logpdfBi, oldlp)//,tmpx0/32.0)
           		case _ => throw new Error("Invalid slice-sampler")
			 }			 
	         for (i <- 0 until hsiters) {
	           val tmp = samplefunc(tmpx0, oldllh)
	           tmpx0 = tmp._1
	           oldllh = tmp._2
	         }
	         tmpx0	          
	      }
	      val biIt = pypBis.values.iterator
	      while (biIt.hasNext) {
	        biIt.next().setConcentration(newSharedBigramAlpha)
	      }
	      concentrationBi = newSharedBigramAlpha
      }
		  
      resampleUni
      resampleBiCoupled
	}	  
	

	def optimizeConcentration = {
      val tmpOptim = new BrentOptimizer(0.00001,0.000001)
      
      	
      def optimUni = {
    	val logpdfUni = new UnivariateFunction {
    	  def value(alpha: Double): Double = {
    	    if (alpha<0)
    		  return Double.NegativeInfinity
	        var result = 0
	        val logPrior =
	          if (wordseg.wordseg.shape != -1)
		        Utils.lgammadistShapeRate(alpha,wordseg.wordseg.shape,wordseg.wordseg.rate)
		      else
		        0
	        pypUni.logProbSeating(alpha)+logPrior
	      }
        }        
        pypUni.concentration = tmpOptim.optimize(20, logpdfUni,org.apache.commons.math3.optimization.GoalType.MAXIMIZE,0.0,32768.0).getPoint()
	    wordseg.wordseg.hyperSampleFile.print(pypUni.concentration-1+" "+logpdfUni.value(pypUni.concentration-1)+"\n")
	    wordseg.wordseg.hyperSampleFile.print(pypUni.concentration+" "+logpdfUni.value(pypUni.concentration)+" <-\n")	      
	    wordseg.wordseg.hyperSampleFile.print(pypUni.concentration+1+" "+logpdfUni.value(pypUni.concentration+1)+"\n")	              
      }
            
	  def optimBiCoupled = {
	      val logpdfBi = new UnivariateFunction {
	        def value(alpha: Double): Double = {
	    	  if (alpha<0)
	            return Double.NegativeInfinity
		      var result = 0
		      val logPrior = 
		        if (wordseg.wordseg.shape != -1)
		          Utils.lgammadistShapeRate(alpha,wordseg.wordseg.shape,wordseg.wordseg.rate)
		        else
		          0
		      var res: Double = 0
		      val bCRPit = pypBis.values.iterator
		      while (bCRPit.hasNext) {
		        res += bCRPit.next().logProbSeating(alpha)
		      }
		      assert(res!=Double.NegativeInfinity)
		      res + logPrior
		  }
	      }
	      val newSharedBigramAlpha = tmpOptim.optimize(50, logpdfBi, org.apache.commons.math3.optimization.GoalType.MAXIMIZE, 0.0, 32768.0).getPoint()
	      val biIt = pypBis.values.iterator	      
	      while (biIt.hasNext) {
	        biIt.next().setConcentration(newSharedBigramAlpha)
	      }
	      concentrationBi = newSharedBigramAlpha
/*	      wordseg.wordseg.hyperSampleFile.print(concentrationBi-1+" "+logpdfBi.value(concentrationBi-1)+"\n")
	      wordseg.wordseg.hyperSampleFile.print(concentrationBi+" "+logpdfBi.value(concentrationBi)+" <-\n")	      
	      wordseg.wordseg.hyperSampleFile.print(concentrationBi+1+" "+logpdfBi.value(concentrationBi+1)+"\n\n")*/	      
      }
      optimUni
      optimBiCoupled  
	}	  

	

	def hyperParam: String = {
	  var res = "alpha0 "+pypUni.concentration
      res += " alpha1 "+concentrationBi
	  res
	}
	  
	def logProb(alpha0: Double, alpha1: Double): Double = {
	  val lp1 = pypUni.base.logProb
	  val lp2 = pypUni.logProbSeating(alpha0)
	  var lp3 = 0.0
	  val pypWIt = pypBis.values.iterator
	  while (pypWIt.hasNext) {
	    val pypW = pypWIt.next()
	    if (wordseg.DEBUG)
	    	assert(pypW.sanityCheck)
	    lp3 += pypW.logProbSeating(alpha1)
	  }
	  val lp4 = if (wordseg.wordseg.hyperparam!="no") {
        Utils.lgammadistShapeRate(pypUni.concentration,wordseg.wordseg.shape,wordseg.wordseg.rate)+
	    Utils.lgammadistShapeRate(concentrationBi,wordseg.wordseg.shape,wordseg.wordseg.rate)
	  } else {
	    0
	  }
	  if (wordseg.DEBUG)
		  println("lp1: "+lp1+"\nlp2: "+lp2+"\nlp3:" +lp3+"\nlp4:"+lp4+"\nlp5:"+data.delModelProb)
	  lp1 + lp2 + lp3 + lp4+data.delModelProb
	}
	
	def logProb: Double = { 
	  val lp1 = pypUni.base.logProb
	  val lp2 = pypUni.logProbSeating
	  var lp3 = 0.0
	  val pypWIt = pypBis.values.iterator
	  while (pypWIt.hasNext) {
	    val pypW = pypWIt.next()
	    if (wordseg.DEBUG)
	    	assert(pypW.sanityCheck)
	    lp3 += pypW.logProbSeating
	  }
	  if (wordseg.DEBUG)
		  println("lp1: "+lp1+"\nlp2: "+lp2+"\nlp3:" +lp3)
	  lp1 + lp2 + lp3 + data.delModelProb + 
	  { if (wordseg.wordseg.hyperparam!="no" && wordseg.wordseg.shape != -1) {
		  Utils.lgammadistShapeRate(pypUni.concentration, wordseg.wordseg.shape, wordseg.wordseg.rate)+
		  Utils.lgammadistShapeRate(concentrationBi, wordseg.wordseg.shape, wordseg.wordseg.rate)
	    } else {
		  0
	    }
	  }
	}
	
	def gibbsSweep(anneal: Double=1.0): Double = {
//	  for (i: Int <- shuffle(1 until boundaries.length)) {
	  for (i: Int <- 1 until boundaries.length) {	    
		  resample(i,anneal)
	  }
	  logProb
	}
	
	/**
	 * only reseat words, and possibly change underlying type
	 */
	def resampleWords(pos: Int, anneal: Double) = {
	  boundaries(pos) match {
	    case NoBoundary => Unit
	    case _ =>
	    val context = boundaryContext(pos)
	    removeAssociatedObservations(context, boundaries(pos),rules(pos))
	    context match {
	    	case BigramMedialContext(leftU,w1O,w1U,w1D,w2O,w2U,w1w2O,w1w2U,rightO,rightU) =>
	    	  val res = __reseatProbs(leftU, w1D, w1O, w2U, w2O, rightU)
	    	  updateBoundary(pos, res.sample(anneal), context)
	    	case BigramFinalContext(leftU,wO,wU,wD) => 
	    	  val res = __reseatProbs(leftU,wD,wO)
	    	  updateBoundary(pos,res.sample(anneal), context)
	  }	      
	  }

	}

	def __reseatProbs(leftU: WordType, w1D: WordType, w1O: WordType): Categorical[(Boundary,Rule)] = {
	  val res = new Categorical[(Boundary,Rule)]
	  if (wordseg.wordseg.dropInferenceMode!=IGNOREDROP)
		  res.add((UBoundary,Del1), predictive(leftU, w1D)*predictive(w1D, data.UBOUNDARYWORD)*toSurface(w1D, w1O,data.UBOUNDARYWORD))
	  val tRule = data.whichTransform(w1O,w1O)
	  res.add((UBoundary,tRule), predictive(leftU, w1O)*predictive(w1O, data.UBOUNDARYWORD))
	  res
	}
	def __reseatProbs(leftU: WordType, w1D: WordType, w1O: WordType, w2U: WordType, w2O: WordType, rightU: WordType): Categorical[(Boundary,Rule)] = {
	  val res = new Categorical[(Boundary,Rule)]
	  if (wordseg.wordseg.dropInferenceMode!=IGNOREDROP)
	    res.add((WBoundary,Del1), predictive(leftU, w1D)*predictive(w1D, w2U)*toSurface(w1D, w1O,w2U)*
			  				 predictive(w2U,rightU)*toSurface(w2U,w2O,rightU))
      val tRule = data.whichTransform(w1O, w1O)			  				 
	  res.add((WBoundary,tRule), predictive(leftU, w1O)*predictive(w1O, w2U)*toSurface(w1O, w1O,w2U)*
			  				 predictive(w2U,rightU)*toSurface(w2U,w2O,rightU))
	  res
	}
	
	/**
	 * only resample words, but determine drops
	 */
	def gibbsSweepWords(anneal: Double=1.0): Double = {
//   for (i: Int <- shuffle(1 until boundaries.length)) {
	  for (i: Int <- 1 until boundaries.length) {
	  	  resampleWords(i,anneal)
	  }
	  logProb
	}*/
}