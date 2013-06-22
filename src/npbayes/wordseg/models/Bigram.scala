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

class Bigram(val corpusName: String,val features: (Int,((WordType,WordType))=>Array[Double]),var concentrationUni: Double,discountUni: Double=0,var concentrationBi: Double, var discountBi: Double=0,val pStop: Double = 0.5, val assumption: HEURISTIC = EXACT,
    		  val dropSeg: String = "KLRK", val dropInd: String = "KLRK",val lexgen: LexGenerator, val phonVar: Boolean=false) extends WordsegModel {
	require(0<=discountUni && discountUni<1)
	require(if (discountUni==0) concentrationUni>0 else concentrationUni>=0)
	
	val unif= new Random
	val data = new Data(corpusName,phonVar,dropInd,dropSeg,"","",features)
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
	
	override def evaluate =
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
	
	override def sanity: Boolean = {
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
	  if (phonVar==false) {
	    if (u==o)
	      1.0
	    else
	      0.0
	  } else
		  data.R(u,o,rU)
	}
	
	def DROPSYMBOL = data.DROP1
	def _phoneSeq = data.data
	
	override def init(gold:Boolean = false, binitProb:Double = 0.5) = { 
	  def inner(bPos: Int): Unit = 
	    if (bPos>data.nBoundaries)
	      Unit
	    else 
	      boundaries(bPos) match {
	      	case false => inner(bPos+1)
	      	case true =>
	      	  val context = data.getBoundaryContext(bPos)
	      	  context match {
	      	    case c: MedialContext =>
	      	      update(c.leftU, c.w1U)
	      	    case c: FinalContext =>
	      	      update(c.leftU,c.w1U)
	      	      update(c.w1U,data.UBOUNDARYWORD)
	      	  }
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
	  val res = inner(1)
	  if (phonVar) data.updateDel1Model
	  if (wordseg.DEBUG)
		  assert(pypUni.sanityCheck)
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
	def _calcMedialHypotheses(context: MedialContext): Categorical[(Boundary,Rule)] = {
	  val res: Categorical[(Boundary,Rule)] = new Categorical
	  res.add((NoBoundary,NoRule),
	      _noBoundary(context.leftU,context.w12U,context.w12O,context.rightU))	 
	  if (phonVar && !wordseg.wordseg.isAnnealing)    
		  res.add((WBoundary,Del1),
		      _boundary(context.leftU,context.w1D1,context.w2U,context.w1O,context.w2O,context.rightU))	    
	  res.add((WBoundary,NoRule),
	      _boundary(context.leftU,context.w1O,context.w2U,context.w1O,context.w2O,context.rightU))
	  assert(res.partition>0)
	  res
	}
	
	def _calcFinalHypotheses(context: FinalContext): Categorical[(Boundary,Rule)] = {
	  val res: Categorical[(Boundary,Rule)] = new Categorical
	  if (phonVar && !wordseg.wordseg.isAnnealing)
		  res.add((UBoundary,Del1),
		      _ubProb(context.leftU,context.w1D1,context.w1O))
	  res.add((UBoundary,NoRule),
	      _ubProb(context.leftU,context.w1O,context.w1O))
	  assert(res.partition>0)
	  res
	}
	
	def updateBoundary(pos: Int, br: (Boundary,Rule), context: AContext) = {
  	  val (b,r) = br
	  val boundary = boundaries(pos)
	  context match {
	    case c: MedialContext =>
	      b match {
	        case WBoundary =>
	          r match {
	            case Del1 =>
	              update(c.leftU,c.w1D1)
	              data.insertWord(c.w1Start, (c.w1D1,c.w1O))
	              update(c.w1D1,c.w2U)
	              data.insertWord(pos,(c.w2U,c.w2O))
	              update(c.w2U,c.rightU)
	            case NoRule =>
	              update(c.leftU,c.w1O)
	              data.insertWord(c.w1Start, (c.w1O,c.w1O))
	              update(c.w1O,c.w2U)
	              data.insertWord(pos,(c.w2U,c.w2O))
	              update(c.w2U,c.rightU)
	          }  
	        case NoBoundary =>
	          update(c.leftU,c.w12U)
	          data.insertWord(c.w1Start, (c.w12U,c.w12O))
	          update(c.w12U,c.rightU)
	      }
	    case c: FinalContext =>
	          r match {
	            case Del1 =>
	              update(c.leftU,c.w1D1)
	              data.insertWord(c.w1Start, (c.w1D1,c.w1O))
	              update(c.w1D1,data.UBOUNDARYWORD)
	            case NoRule =>
	              update(c.leftU,c.w1O)
	              data.insertWord(c.w1Start, (c.w1O,c.w1O))
	              update(c.w1O,data.UBOUNDARYWORD)	              
	          }
	  }
	}
	
	def removeAssociatedObservations(context: AContext, boundary: Boolean) = 
	  context match {
	  case c: MedialContext =>
	    boundary match {
	      case true =>
	        removeWrap(c.leftU,c.w1U)
	        removeWrap(c.w1U,c.w2U)
	        removeWrap(c.w2U,c.rightU)
	      case false =>
	        removeWrap(c.leftU,c.w12U)
	        removeWrap(c.w12U,c.rightU)
	    }
	  case c: FinalContext =>
	  	removeWrap(c.leftU,c.w1U)
	  	removeWrap(c.w1U,data.UBOUNDARYWORD)
	}
	
	def _calcHypotheses(context: AContext): Categorical[(Boundary,Rule)] = context match {
	  case c: MedialContext => _calcMedialHypotheses(c)
	  case c: FinalContext => _calcFinalHypotheses(c)
	}
	
	def resample(pos: Int, anneal: Double=1.0): Unit = {
	  val boundary = data.boundaries(pos)
	  val context = data.removeBoundary(pos)
	  removeAssociatedObservations(context, boundary)
	  val result = _calcHypotheses(context)
	  val (newBound,newRule) = {
		  if (anneal==1.0)
			result.sample
		  else
			result.sample(anneal)
		  }
	  updateBoundary(pos, (newBound,newRule),context)
	}
	
	override def resampleConcentration(hsiters: Int = 1) = {
	
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
	      pypUni.propLogProb(alpha)+logPrior
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
            
	  def resampleBi = {
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
		        res += bCRPit.next().propLogProb(alpha)
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
      resampleBi
	}	  
	

	override def optimizeConcentration = {
		def optimUni =  {
		    def logpdfUni(alpha: Double): Double = 
	    	    if (alpha<0)
	    		  Double.NegativeInfinity
	    		else {
			        var result = 0
			        val logPrior =
			          if (wordseg.wordseg.shape != -1)
				        Utils.lgammadistShapeRate(alpha,wordseg.wordseg.shape,wordseg.wordseg.rate)
				      else
				        0
			        pypUni.logProbSeating(alpha)+logPrior
	    		}     
	        //pypUni.concentration = tmpOptim.optimize(20, logpdfUni,org.apache.commons.math3.optimization.GoalType.MAXIMIZE,0.0,32768.0).getPoint()
	    	pypUni.concentration = optimizer.approxGradientDescent1D(pypUni.concentration, logpdfUni, 0.0001, 100, 0.01)
		    wordseg.wordseg.hyperSampleFile.print(pypUni.concentration-1+" "+logpdfUni(pypUni.concentration-1)+"\n")
		    wordseg.wordseg.hyperSampleFile.print(pypUni.concentration+" "+logpdfUni(pypUni.concentration)+" <-\n")	      
		    wordseg.wordseg.hyperSampleFile.print(pypUni.concentration+1+" "+logpdfUni(pypUni.concentration+1)+"\n")	              
      }
            
	  def optimBiCoupled = {
	      def logpdfBi(alpha: Double): Double = {
	    	  if (alpha<0)
	            Double.NegativeInfinity
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
	      val newSharedBigramAlpha = optimizer.approxGradientDescent1D(concentrationBi, logpdfBi, 0.0001, 100, 0.5)
	      val biIt = pypBis.values.iterator	      
	      while (biIt.hasNext) {
	        biIt.next().setConcentration(newSharedBigramAlpha)
	      }
	      concentrationBi = newSharedBigramAlpha
	      wordseg.wordseg.hyperSampleFile.print("Bi:\n")
	      wordseg.wordseg.hyperSampleFile.print(concentrationBi-1 + " " + logpdfBi(concentrationBi-1)+"\n")
	      wordseg.wordseg.hyperSampleFile.print(concentrationBi+" "+logpdfBi(concentrationBi)+" <-\n")	      
	      wordseg.wordseg.hyperSampleFile.print(concentrationBi+1+" "+logpdfBi(concentrationBi+1)+"\n\n")	      
      }
      optimUni
      optimBiCoupled  
	}	  

	

	override def hyperParam: String = {
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
		  println("lp1: "+lp1+"\nlp2: "+lp2+"\nlp3:" +lp3+"\nlp4:"+lp4+{if (phonVar) "\nlp5:"+data.delModelProb else ""})
	  lp1 + lp2 + lp3 + lp4+ {if (phonVar) data.delModelProb else 0}
	}
	
	
	def logProbAdaptors = {
	  val lpUni = pypUni.logProbSeating
	  var lpBis = 0.0
	  val pypWIt = pypBis.values.iterator
	  while (pypWIt.hasNext) {
	    val pypW = pypWIt.next()
	    if (wordseg.DEBUG)
	    	assert(pypW.sanityCheck)
	    lpBis += pypW.logProbSeating
	  }
	  lpUni+lpBis	  
	}
	
	def logProbPrior = if (wordseg.wordseg.hyperparam!="no") {
	     					Utils.lgammadistShapeScale(pypUni.concentration,wordseg.wordseg.shape,wordseg.wordseg.rate)+
	     					Utils.lgammadistShapeScale(concentrationBi,wordseg.wordseg.shape,wordseg.wordseg.rate) 					
						}
	     				else
	      					0
	
	def logProbGenerator = pypUni.base.logProb
	
	override def logProb: Double = { 
	  val lp1 = pypUni.base.logProb
	  val lp2 = pypUni.logProbSeating
	  val logprior = if (wordseg.wordseg.hyperparam!="no" && wordseg.wordseg.shape != -1) {
		  Utils.lgammadistShapeRate(pypUni.concentration, wordseg.wordseg.shape, wordseg.wordseg.rate)+
		  Utils.lgammadistShapeRate(concentrationBi, wordseg.wordseg.shape, wordseg.wordseg.rate)
	    } else {
		  0
	    }
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
//	  System.err.println("lp1: "+(lp1+lp2+lp3)+" lp2: "+data.delModelProb)
	  println("lp1: "+lp1+" lp2: "+lp2+" lp3:" +lp3+ " lprior: "+logprior)
		  
	  lp1 + lp2 + lp3 + data.delModelProb + logprior
	}
	
	override def gibbsSweep(anneal: Double=1.0): Double = {
//	  for (i: Int <- shuffle(1 until data.nBoundaries)) {
	  for (i: Int <- 1 until data.nBoundaries) {	    
		  resample(i,anneal)
	  }
	  logProb
	}
	
	/**
	 * only reseat words, and possibly change underlying type
	 */
	def resampleWords(pos: Int, anneal: Double) = {
	  boundaries(pos) match {
	    case false => Unit
	    case true =>
	    	val context = data.removeBoundary(pos)
	    	removeAssociatedObservations(context,true) 
	    context match {
	    	case c: MedialContext =>
	    	  val res = __reseatProbs(c.leftU, c.w1D1, c.w1O, c.w2U, c.w2O, c.rightU)
	    	  updateBoundary(pos, res.sample(anneal), context)
	    	case c: FinalContext => 
	    	  val res = __reseatProbs(c.leftU,c.w1D1,c.w1O)
	    	  updateBoundary(pos,res.sample(anneal), context)
	    }	      
	  }

	}

	def __reseatProbs(leftU: WordType, w1D: WordType, w1O: WordType): Categorical[(Boundary,Rule)] = {
	  val res = new Categorical[(Boundary,Rule)]
	  if (phonVar)
		  res.add((UBoundary,Del1), predictive(leftU, w1D)*predictive(w1D, data.UBOUNDARYWORD)*toSurface(w1D, w1O,data.UBOUNDARYWORD))
	  res.add((UBoundary,NoRule), predictive(leftU, w1O)*predictive(w1O, data.UBOUNDARYWORD))
	  res
	}
	def __reseatProbs(leftU: WordType, w1D: WordType, w1O: WordType, w2U: WordType, w2O: WordType, rightU: WordType): Categorical[(Boundary,Rule)] = {
	  val res = new Categorical[(Boundary,Rule)]
	  if (phonVar)
	    res.add((WBoundary,Del1), predictive(leftU, w1D)*predictive(w1D, w2U)*toSurface(w1D, w1O,w2U)*
			  				 predictive(w2U,rightU)*toSurface(w2U,w2O,rightU))
	  res.add((WBoundary,NoRule), predictive(leftU, w1O)*predictive(w1O, w2U)*toSurface(w1O, w1O,w2U)*
			  				 predictive(w2U,rightU)*toSurface(w2U,w2O,rightU))
	  res
	}
	
	/**
	 * only resample words, but determine drops
	 */
	override def gibbsSweepWords(anneal: Double=1.0): Double = {
      for (i: Int <- shuffle(1 to data.nBoundaries)) {
	  //for (i: Int <- 1 until data.nBoundaries) {
	  	  resampleWords(i,anneal)
	  }
	  if (phonVar) wordseg.wordseg.loglearn match {
	    case "optimize" => data.updateDel1Model 
	    case "sample" => data.updateDel1ModelSample
	    case _ => throw new Error("--loglearn has to be either 'optimize' or 'sample'")
	  }
	  logProb
	}
	
	def totalTables = pypUni._tCount + {
	  var res = 0
	  val it = pypBis.values().iterator()
	  while (it.hasNext()) {
	    res += it.next()._tCount
	  }
	  res
	}
}