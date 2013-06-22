package npbayes.wordseg

import scala.collection.immutable.Map
import scala.io.Source
import npbayes.wordseg.models.Unigram
import npbayes.wordseg.models.Bigram
import npbayes.wordseg.models.WordsegModel
import npbayes.ArgParser
import npbayes.distributions.{EXACT,MAXPATH,MINPATH}
import npbayes.distributions.PosteriorPredictive
import npbayes.wordseg.data.Identifier
import npbayes.wordseg.lexgens.MonkeyUnigram
import npbayes.wordseg.data._
import com.sun.org.apache.xerces.internal.util.SymbolTable
import npbayes.wordseg.lexgens.MonkeyBigram
import npbayes.wordseg.lexgens.UnigramLearned
import npbayes.wordseg.lexgens.BigramLearned
import scala.collection.mutable.LinkedList
import scala.collection.mutable.ListBuffer
import java.io.PrintStream
import npbayes.distributions.MAXPROB
import npbayes.maxent.LogisticRegression
import breeze.linalg.DenseVector
import npbayes.WordType

abstract class LexGenerator
case object UNIFORM extends LexGenerator
case object UNIUNLEARNED extends LexGenerator
case object UNILEARNED extends LexGenerator
case object UNILEARNEDVOWELS extends LexGenerator
case object BIUNLEARNED extends LexGenerator
case object BILEARNED extends LexGenerator
case object BILEARNEDVOWELS extends LexGenerator
/*import npbayes.LexGenerator
import npbayes.BIUNLEARNED
import npbayes.BILEARNEDVOWELS
import npbayes.BILEARNED
import npbayes.UNILEARNEDVOWELS
import npbayes.UNILEARNED
import npbayes.UNIUNLEARNED*/ 


/**
 * vanilla-word segmentation
 * @author bborschi
 *
 */


abstract class DROPINFERENCEMODE
case class FIXDROP(val prob: Double) extends DROPINFERENCEMODE
case class INFERDROP(val dropPrior: Double, val noDropPrior: Double) extends DROPINFERENCEMODE
case object IGNOREDROP extends DROPINFERENCEMODE



class TaggerParams(args: Array[String]) extends ArgParser(args) {
	
	def NGRAM = getString("--ngram", "2");	params += (("ngram",NGRAM))
	def ALPHA0 = getDouble("--alpha0", 100); params += (("alpha0",ALPHA0))
	def ALPHA1 = getDouble("--alpha1", 3000); params += (("alpha1",ALPHA1))
	def PSTOP = getDouble("--pstop", 0.5); params += (("pstop",PSTOP))	//monkey-model stop-probability
	def LEXGEN = getString("--lexgen","monkey"); params += (("lexgen",LEXGEN)) //monkey, learned, vowel
	def PHONMAP = getString("--phonmap",""); params += (("phonmap",PHONMAP))
	def ITERS = getInt("--iters",1000); params += (("iters",ITERS))
	def ANNEALITERS = getInt("--annealIters",1000); params += (("annealIters",ANNEALITERS))
	def STARTTEMP = getDouble("--startTemp",1); params += (("startTemp",STARTTEMP))
	def STOPTEMP = getDouble("--stopTemp",1); params += (("stopTemp",STOPTEMP))
	def PHONVAR = getBoolean("--phonVar",false); params += (("phonVar",PHONVAR)) //if set to -1, do inference, if set to 0.0, ignore
	def DROPSEG = getString("--dropSeg","NONESEG1"); params += (("dropSeg",DROPSEG)) //what is the dropSegment (segment that actually occurs)
	def DROPIND = getString("--dropInd","NONEIND1"); params += (("dropInd",DROPIND)) //what is the indicator (for evaluation)
	def INPUT = getString("--input",""); params += (("input",INPUT))
	def OUTPUT = getString("--output",""); params += (("output",OUTPUT))
	def TRACE = getString("--trace",""); params += (("trace",TRACE))
	def ASSUMPTION = getString("--assumption","EXACT"); params += (("assumption",ASSUMPTION))
	def GOLDINIT = getBoolean("--goldinit",false); params += (("goldinit",GOLDINIT))
	def MODE = getString("--mode","WORDSEG"); params += (("mode",MODE))
	def BURNIN = getInt("--burnin",2000); params += (("burnin",BURNIN))
	def SAMPLES = getInt("--sampleEvery",10); params += (("sampleEvery",SAMPLES))
	def BOUNDINITPROB = getDouble("--binitProb",0.0); params += (("binitProb",BOUNDINITPROB))
	def FEATURES = getString("--features","no"); params += (("features",FEATURES))
	def LOGLEARN = getString("--loglearn","optimize"); params += (("loglearn",LOGLEARN))
	def DELAYRECOVERY = getBoolean("--delayRecovery",true); params += (("delayRecovery",DELAYRECOVERY))  //if set to true, no recovery during annealing
	def HYPERPARAM = getString("--hyper","no"); params += (("hyper",HYPERPARAM))
	def SHAPE = getDouble("--shape",0.1); params += (("shape",SHAPE))
	def RATE = getDouble("--rate",0.1); params += (("rate",RATE))
	def HSAMPLE = getString("--hsampler","slice"); params += (("hsampler",HSAMPLE))
	def HSAMPLEITERS = getInt("--hsampleiters",10); params += (("hsampleiters",HSAMPLEITERS))
	def HSMHSSD = getDouble("--hsmhsd",0.1); params += (("hsmhsd",HSMHSSD))
	def HSLOWITERS = getInt("--hslowiters",0); params += (("hslowiters",HSLOWITERS))
}

object wordseg {
	var hyperSampleFile: PrintStream = null
    var dropInferenceMode: DROPINFERENCEMODE = IGNOREDROP
    var isConsonant: Identifier = null
    def isVowel(x: SegmentType) = SymbolClassTable(PhonemeClassMap.getClass(x))=="VOWL"
    var isPause: Identifier = null
//    var data: VarData = null
    var isAnnealing: Boolean = false //so we can check whether we are still annealing or not
    var shape: Double = 0.1
    var rate: Double = 0.1
    var hyperparam: String = "no"
    var hsample: String = null
    var hsampleiters: Int = 0
    var hsmhvar: Double = 0.1
    var binitProb: Double = 0.0
    var loglearn: String = "optimize"
    var features: (Int, ((WordType,WordType))=>Array[Double]) = null
	def main(args: Array[String]) {
	  val options = new TaggerParams(args)
	  	if (options.PHONMAP!="") {
	  //PhonemeClassMap.init(options.PHONMAP)
	  PhonemeFeatureMap.init(options.PHONMAP)
	}
	  val assumption = options.ASSUMPTION match {
	    case "EXACT" =>
	      EXACT
	    case "MINPATH" =>
	      MINPATH
	    case "MAXPATH" =>
	      MAXPATH
	    case "MAXPROB"=>
	      MAXPROB
	    case _ =>
	      throw new Error(options.ASSUMPTION+ " is invalid value for --asumption: either EXACT, MINPATH or MAXPATH")
	  }
	  loglearn = options.LOGLEARN
	features = options.FEATURES match {
	  case "no" => Features.featuresNo
	  case "right" => Features.featuresN
	  case "leftright" => Features.featuresPN
	  case "leftrightident" => Features.featuresPNIdent
	  case "interaction" => Features.featuresInteraction
	  case "bigset" => Features.featuresPNInteraction
	  case "coetzee" => Features.featuresCoetzee
	  case "large" => Features.featuresLarge2
	  case "largeNext" => Features.featuresLargeNext
	}
	hsampleiters = options.HSAMPLEITERS
	hsample = options.HSAMPLE
	hsmhvar = options.HSMHSSD

	binitProb = options.BOUNDINITPROB
	
	  

	  shape = options.SHAPE
	  rate = options.RATE
	  hyperparam = options.HYPERPARAM
//	  data = new VarData(options.INPUT,options.DROPPROB,options.DROPIND,options.DROPSEG,contextModel)
	  
	  val lexgen: LexGenerator = options.LEXGEN match {
	  	case "uniform" =>
	  		UNIFORM
	    case "monkey" =>
	    	options.NGRAM match {
	    	  case "1" =>
	    	    UNIUNLEARNED
	    	  case "2" =>
	    	    BIUNLEARNED
	    	}
	    case "learn" =>
	      options.NGRAM match {
	        case "1" =>
	         UNILEARNED
	        case "2" =>
	          BILEARNED
	      }
	    case "vowel" =>
	      options.NGRAM match {
	        case "1" =>
	          UNILEARNEDVOWELS
	        case "2" =>
	          BILEARNEDVOWELS
	      }
	  } 

	  val phonVar = options.PHONVAR
	  val model: WordsegModel = options.NGRAM match {
	    case "1" =>
	      new Unigram(options.INPUT,features,options.ALPHA0,0,options.PSTOP,assumption,options.DROPSEG,options.DROPIND,lexgen,phonVar)
	    case "2" =>
	      new Bigram(options.INPUT,features,options.ALPHA0,0,options.ALPHA1,0, options.PSTOP,assumption,options.DROPSEG,options.DROPIND,lexgen,phonVar)	      
	  }
	  
	  def annealTemperature(x: Int) = 	    //npbayes.wordseg.annealTemperature(x)
		npbayes.wordseg.annealTemperature(options.STARTTEMP, options.ANNEALITERS, 1)(x)
	  
	  def sample = options.MODE match {
	    case "WORDSEG" =>
	      model.gibbsSweep(_)
	    case "LANGMODEL" =>
	      model.gibbsSweepWords(_)
	  }
	  
	  def prettyString(x: DenseVector[Double]): String = {
	    val res = new ListBuffer[Double]
	    var i = 0
	    while (i<x.size) {
	      res += x(i)
	      i+=1
	    }
	    res.mkString(" ")
	  }
	  val weightFile: PrintStream = if (phonVar)
	    new java.io.PrintStream(new java.io.File(options.OUTPUT+".weights"),"utf-8")
	  else
	    null
	  val traceFile = new java.io.PrintStream(new java.io.File(options.OUTPUT+".trace"),"utf-8")
	  val evalFile = new java.io.PrintStream(new java.io.File(options.OUTPUT+".eval"),"utf-8")
	  val sampleFile = new java.io.PrintStream(new java.io.File(options.OUTPUT+".samples"),"utf-8")
	  hyperSampleFile = new java.io.PrintStream(new java.io.File(options.OUTPUT+".hypersamples"),"utf-8")
	  println(options)
	  traceFile.println(options)
	  model.init(options.GOLDINIT,options.BOUNDINITPROB)

	   val log = "0 "+ //iteration
	    		  options.STARTTEMP+" "+ //temperature
	    		  model.logProb+" "+//total logprob
	    		  model.logProbAdaptors+" "+//Adaptor logprobs
	    		  model.logProbGenerator+" "+//Generator logprobs
	    		  model.logProbPrior+ " "+//prior logprob
	    		  {if (phonVar) model.data.delModel1.loglikelihood()+" " else ""}+ //deletionmodel	    		  
	    		  model.totalTables+ " "+//total number of tables
	    		  model.hyperParam //hyper-parameters

	    		 
	  println(log); traceFile.println(log)
	  if (phonVar)
	    weightFile.println(Features.featureLargeName2.mkString(" "))
	    	  
	  for (i <- 1 to options.ITERS) {
	    val temperature: Double = annealTemperature(i)
	    isAnnealing = temperature!=1.0
	    sample(1/temperature)
	    if (i>4 && i % 1 == 0) hyperparam match {
	      case "no" =>
	      case "sample" => model.resampleConcentration({if (i<options.HSLOWITERS) 1 else options.HSAMPLEITERS})
	      case "optimize" => model.optimizeConcentration
	    }
	    
	    val log = i+" "+ //iteration
	    		  temperature+" "+ //temperature
	    		  model.logProb+" "+//total logprob
	    		  model.logProbAdaptors+" "+//Adaptor logprobs
	    		  model.logProbGenerator+" "+//Generator logprobs
	    		  model.logProbPrior+ " "+//prior logprob
	    		  {if (phonVar) model.data.delModel1.loglikelihood()+" " else ""}+ //deletionmodel
	    		  model.totalTables+ " "+//total number of tables
	    		  model.hyperParam //hyper-parameters
/*	    		  " "+model.evaluate+
	    		{ if (phonVar)
	    			prettyString(model.data.delModel1.weights)
	    		  else "" } +
	    	  " " + model.hyperParam + {if (options.NGRAM=="1") " "+model.uniTables + " " + model.uniCustomers else ""}*/
	    println(log); traceFile.println(log); evalFile.println(i+" "+model.evaluate)
	    if (phonVar) weightFile.println(prettyString(model.data.delModel1.weights))
	    if (i>=options.BURNIN && i%options.SAMPLES==0) {
	      model.writeAnalysis(sampleFile)
	      sampleFile.println()
	    }
	  }
	  model.writeAnalysis(new java.io.PrintStream(new java.io.File(options.OUTPUT+".out")))
	  traceFile.close()
	  sampleFile.close()
	}
}