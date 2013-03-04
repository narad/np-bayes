package npbayes.wordseg

import scala.collection.immutable.Map
import scala.io.Source
import npbayes.wordseg.models.Unigram
import npbayes.wordseg.models.Bigram
import npbayes.wordseg.models.WordsegModel
import npbayes.ArgParser
import npbayes.distributions.{EXACT,MAXPATH,MINPATH}
import npbayes.wordseg.data.GlobalFix
import npbayes.wordseg.data.CVPFollows
import npbayes.distributions.PosteriorPredictive
import npbayes.wordseg.data.Identifier
import npbayes.wordseg.lexgens.MonkeyUnigram
import npbayes.wordseg.data._
import com.sun.org.apache.xerces.internal.util.SymbolTable
import npbayes.wordseg.lexgens.MonkeyBigram
import npbayes.wordseg.lexgens.MonkeyBigram
import npbayes.wordseg.lexgens.UnigramLearned
import npbayes.wordseg.lexgens.BigramLearned


/**
 * vanilla-word segmentation
 * @author bborschi
 *
 */


abstract class DROPINFERENCEMODE
case class FIXDROP(val prob: Double) extends DROPINFERENCEMODE
case class INFERDROP(val dropPrior: Double, val noDropPrior: Double) extends DROPINFERENCEMODE
case class IGNOREDROP extends DROPINFERENCEMODE



class TaggerParams(args: Array[String]) extends ArgParser(args) {
	def NGRAM = getString("--ngram", "2")
	def ALPHA0 = getDouble("--alpha0", 100)
	def ALPHA1 = getDouble("--alpha1", 3000)
	def PSTOP = getDouble("--pstop", 0.5)	//monkey-model stop-probability
	def LEXGEN = getString("--lexgen","monkey") //monkey, learned, vowel
	def VOWELLIST = getString("--vowels","/home/bborschi/git/TDropping/vowels.txt")
	def CONSLIST = getString("--consonants","/home/bborschi/git/TDropping/consonants.txt")
	def SILLIST = getString("--pauses","/home/bborschi/git/TDropping/silences.txt")
	def ITERS = getInt("--iters",1000)
	def ANNEALITERS = getInt("--annealIters",1000)
	def STARTTEMP = getDouble("--startTemp",1)
	def STOPTEMP = getDouble("--stopTemp",1)
	def DROPPROB = getDouble("--dropProb",0.0) //if set to -1, do inference, if set to 0.0, ignore
	def DROPSEG = getString("--dropSeg","KRLKR") //what is the dropSegment (segment that actually occurs)
	def DROPIND = getString("--dropInd","KRLKR") //what is the indicator (for evaluation)
	def INPUT = getString("--input","")
	def OUTPUT = getString("--output","")
	def TRACE = getString("--trace","")
	def ASSUMPTION = getString("--assumption","EXACT")
	def GOLDINIT = getBoolean("--goldinit",false)
	def MODE = getString("--mode","WORDSEG")
	def BURNIN = getInt("--burnin",2000)
	def SAMPLES = getInt("--sampleEvery",10)
	def BOUNDINITPROB = getDouble("--binitProb",0.0)
	def DROPPRIOR = getDouble("--dropPrior",1.0)
	def NODROPPRIOR = getDouble("-noDropPrior",1.0)
	def CONTEXTMODEL = getString("--context","no")
	def DELAYRECOVERY = getBoolean("--delayRecovery",true)  //if set to true, no recovery during annealing
	def HYPERPARAM = getBoolean("--hyper",false)
	def SHAPE = getDouble("--shape",0.1)
	def RATE = getDouble("--shape",0.1)
}

object wordseg {
    var dropInferenceMode: ScalaObject = null
    var isConsonant: Identifier = null
    var isVowel: Identifier = null
    var isPause: Identifier = null
    var data: VarData = null
    var isAnnealing: Boolean = false //so we can check whether we are still annealing or not
    var shape: Double = 0.1
    var rate: Double = 0.1
    var hyperparam: Boolean = false
    
    
	def main(args: Array[String]) = {
	  val options = new TaggerParams(args)
	  val assumption = options.ASSUMPTION match {
	    case "EXACT" =>
	      EXACT
	    case "MINPATH" =>
	      MINPATH
	    case "MAXPATH" =>
	      MAXPATH
	    case _ =>
	      throw new Error(options.ASSUMPTION+ " is invalid value for --asumption: either EXACT, MINPATH or MAXPATH")
	  }
	
	isConsonant = new Identifier(options.CONSLIST)
 	isVowel = new Identifier(options.VOWELLIST)
	isPause = new Identifier(options.SILLIST)
	  val contextModel = options.CONTEXTMODEL match {
	    case "no" =>
	      GlobalFix
	    case "right" =>
	      CVPFollows
	    case "leftright" =>
	      CVPLeftRight
	    case _ =>
	      throw new Error(options.CONTEXTMODEL+" is invalid value for --context: either no or right or leftright")
	  }
	  
	  dropInferenceMode = options.DROPPROB match {
	    case 0.0 => IGNOREDROP
	    case -1.0 => INFERDROP(options.DROPPRIOR,options.NODROPPRIOR)
	    case _ => FIXDROP(options.DROPPROB)
	  }
	  shape = options.SHAPE
	  rate = options.RATE
	  hyperparam = options.HYPERPARAM
	  data = new VarData(options.INPUT,options.DROPPROB,options.DROPIND,options.DROPSEG,contextModel)
	  
	  val lexgen: PosteriorPredictive[WordType] = options.LEXGEN match {
	    case "monkey" =>
	    	options.NGRAM match {
	    	  case "1" =>
	    	    new MonkeyUnigram(npbayes.wordseg.data.SymbolTable.nSymbols-2,0.5)
	    	  case "2" =>
	    	    new MonkeyBigram(npbayes.wordseg.data.SymbolTable.nSymbols-2,0.5,data.UBOUNDARYWORD,0.5)
	    	}
	    case "learn" =>
	      options.NGRAM match {
	        case "1" =>
	          new UnigramLearned(npbayes.wordseg.data.SymbolTable.nSymbols-2,0.1)
	        case "2" =>
	          new BigramLearned(npbayes.wordseg.data.SymbolTable.nSymbols-2,data.UBOUNDARYWORD,0.5,0.1,false)
	      }
	    case "vowel" =>
	      options.NGRAM match {
	        case "1" =>
	          new UnigramLearned(npbayes.wordseg.data.SymbolTable.nSymbols-2,0.1)
	        case "2" =>
	          new BigramLearned(npbayes.wordseg.data.SymbolTable.nSymbols-2,data.UBOUNDARYWORD,0.5,0.1,true)
	      }
	  } 
	  
	  val model: WordsegModel = options.NGRAM match {
	    case "1" =>
	      new Unigram(options.INPUT,options.ALPHA0,0,options.PSTOP,assumption,options.DROPSEG,options.DROPIND,options.DROPPROB,contextModel,lexgen)
	    case "2" =>
	      new Bigram(options.INPUT,options.ALPHA0,0,options.ALPHA1,0, options.PSTOP,assumption,options.DROPSEG,options.DROPIND,options.DROPPROB,contextModel,lexgen)	      
	  }
	  
	  def annealTemperature(x: Int) = 	    //npbayes.wordseg.annealTemperature(x)
		npbayes.wordseg.annealTemperature(options.STARTTEMP, options.ANNEALITERS, 1)(x)
	  
	  def sample = options.MODE match {
	    case "WORDSEG" =>
	      model.gibbsSweep(_)
	    case "LANGMODEL" =>
	      model.gibbsSweepWords(_)
	  }
	  val traceFile = new java.io.PrintStream(new java.io.File(options.OUTPUT+".trace"))
	  val sampleFile = new java.io.PrintStream(new java.io.File(options.OUTPUT+".samples"))
	  println(options)
	  traceFile.println(options)
	  model.init(options.GOLDINIT,options.BOUNDINITPROB)
	  
	  println(0+" "+1+" "+model.logProb+" "+model._logProbTrack+" "+model.evaluate +   	{if (dropInferenceMode==IGNOREDROP)
	    	  " -1"
	    	else
	    	  " " + model.data.showDropProbs} +
	    	  " " + model.hyperParam)
	  traceFile.println(0+" "+1+" "+model.logProb+" "+model._logProbTrack+" "+model.evaluate +   	{if (dropInferenceMode==IGNOREDROP)
	    	  " -1"
	    	else
	    	  " " + model.data.showDropProbs} +
	    	  " " + model.hyperParam)
	    	  
	  for (i <- 1 to options.ITERS) {
	    val temperature: Double = annealTemperature(i)
	    isAnnealing = temperature!=1.0
	    sample(1/temperature)
	    val log = i+" "+temperature+" "+model.logProb+" "+" "+model.evaluate +
	    	{if (dropInferenceMode==IGNOREDROP)
	    	  " -1"
	    	else
	    	  " " + model.data.showDropProbs} +
	    	  " " + model.hyperParam
	    println(log); traceFile.println(log)
	    if (hyperparam)
	    	model.resampleConcentration
	    if (i>=options.BURNIN && i%options.SAMPLES==0) {
	      model.writeAnalysisB(sampleFile)
	      sampleFile.println()
	    }
	  }
	  model.writeAnalysisB(new java.io.PrintStream(new java.io.File(options.OUTPUT+".out")))
	  traceFile.close()
	  sampleFile.close()
	}
}