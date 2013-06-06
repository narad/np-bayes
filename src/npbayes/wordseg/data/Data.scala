package npbayes.wordseg.data


import java.io._
import scala.collection.mutable.HashMap
import scala.io.Source
import scala.util.Random
import scala.collection.mutable.StringBuilder
import com.google.common.collect.ImmutableList.Builder
import org.apache.commons.math3.special.Gamma
import npbayes.wordseg.IGNOREDROP
import npbayes.wordseg.FIXDROP
import npbayes.wordseg.INFERDROP
import scala.collection.mutable.ArrayBuffer
import npbayes.WordType
import breeze.linalg.DenseVector
import npbayes.maxent.LogisticRegression
import scala.collection.mutable.BitSet




abstract class Rule
case object Del1 extends Rule
case object Del2 extends Rule
case object NoRule extends Rule

abstract class Boundary
case object NoBoundary extends Boundary
case object WBoundary extends Boundary
case object UBoundary extends Boundary


abstract class PhonemeType
case class PType(x: Int) extends PhonemeType {
  override def toString = SymbolClassTable(x)
}
case object Consonant extends PhonemeType
case object Vowel extends PhonemeType
case object Pause extends PhonemeType

class Identifier(f: String) {
  var itemsT: Set[SegmentType] = Set[SegmentType]()
  for (l <- scala.io.Source.fromFile(f).getLines)
    itemsT = itemsT+SymbolSegTable(l)
  
  def apply(x: SegmentType): Boolean =
    itemsT.contains(x)
  
  def example: SegmentType =
    itemsT.head 
}


abstract class AContext
case class MedialContext(leftU: WordType,w1U: WordType, w1O: WordType, w1D1: WordType, w1D2: WordType,
					w2U: WordType, w2O: WordType,w12U: WordType, w12O: WordType, rightU: WordType,
					w1Start: Int) extends AContext

case class FinalContext(leftU: WordType, w1U: WordType, w1O: WordType, w1D1: WordType, w1D2: WordType,
						w1Start: Int) extends AContext					

object Data {
   
  /**
    * coarse previous-next feature set 
    * (prevVwl, prevCons, nextVwl, nextCons, nextPaus)
    **/ 
   val featuresPN = (5, {def x(w1w2: (WordType,WordType)): Array[Double] = {
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

  /**
    * coarse next feature set 
    * (nextVwl, nextCons, nextPaus)
    **/ 
   val featuresN = (3, {def x(w1w2: (WordType,WordType)): Array[Double] = {
	    val (w1,w2) = w1w2
	    val res = Array.fill[Double](3)(0.0)
	    val w2first = PhonemeClassMap.getClass(w2(0))
	    res(0) = SymbolClassTable(w2first) match {
	  					  case "Vwl" => 1.0
	  					  case _ => 0.0
	    		 }    
	    res(1) = SymbolClassTable(w2first) match {
	  					  case "Cons" => 1.0
	  					  case _ => 0.0
	    		 }        
	    res(2) = SymbolClassTable(w2first) match {
	  					  case "Pause" => 1.0
	  					  case _ => 0.0
	    		 }        
	    res
   	}
   x(_)
  })
}


class Data(fName: String, val dropProb: Double = 0.0,val MISSING1: String = "*", val DROP1: String = "T", val MISSING2: String="DROPD", val DROP2: String = "D", val nAndFeatures: (Int, ((WordType,WordType))=>Array[Double])=Data.featuresPN) {
	val _random = new Random()
	//Special Characters
	val UBOUNDARYSYMBOL="UTTERANCEBOUNDARY"
	val UBOUNDARYWORD=new WordType(Array.fill(1)(SymbolSegTable(UBOUNDARYSYMBOL)),0,1,-1) //always prefix corpus with an additional boundary symbol	  
	val DROPSEG1=SymbolSegTable(DROP1)
	val DROPSEG2=SymbolSegTable(DROP2)
	
	//logistic regressions
	val (nFeatures,features) = nAndFeatures
	val delModel1 = new LogisticRegression[(WordType,WordType)](nFeatures,features)
	val delModel2 = new LogisticRegression[(WordType,WordType)](nFeatures,features)
	
	//data-structures
	val boundaries = BitSet.empty+0	
	val (data,goldboundaries,uboundaries,golddel1s,golddel2s,nBoundaries) = init	
    val words = new HashMap[Int,(WordType,WordType)] //Underlying, Observed
	val del1s: BitSet = new BitSet() //indicates deletion of Segment1
	val del2s: BitSet = new BitSet() //indicates deletion of Segment2
	
	/**
	 * Initialize the data and goldBoundaries
	 */
	

	def init() = {
		var seqPhones = Vector.empty[Int]
		val uBoundaries: BitSet = BitSet.empty+0 //always uBoundary at beginning
		val goldboundaries: BitSet = BitSet.empty+0 //always uBoundary at beginning
		val golddel1s: BitSet = new BitSet()
		val golddel2s: BitSet = new BitSet()
		var bPos = 0
		var stringPos = 0
		def processLine(line: String) = {
			for (w <- line.stripLineEnd.split("\t")) {
				for (c: String <- w.split(" ")) {
					seqPhones = seqPhones:+ SymbolSegTable(c)
					stringPos+=1
					bPos+=1
				}
				//check for deleted symbols
				SymbolSegTable(seqPhones.last) match {
				  case MISSING1 =>
				    seqPhones = seqPhones.dropRight(1)
				    bPos-=1
				    golddel1s+=bPos
				  case MISSING2 =>
				    seqPhones = seqPhones.dropRight(1)
				    bPos-=1
				    golddel2s+=bPos				    				    
				  case _ =>	    
				}
				goldboundaries+=bPos				
			}
		   uBoundaries+=bPos
		}
		for (l <- Source.fromInputStream(new FileInputStream(fName), "utf-8").getLines) processLine(l)
		(seqPhones.toArray,goldboundaries,uBoundaries,golddel1s,golddel2s,bPos)
	}

	def whichTransform(observed: WordType, underlying: WordType): Rule = {
	  val uEndsIn = underlying.lastSeg
	  val sameLength = observed.size==underlying.size
	  if (underlying.size>1) {
	    uEndsIn match {
	      case DROPSEG1 => if (sameLength) NoRule else Del1
	      case DROPSEG2 => if (sameLength) NoRule else Del2 
	      case _ => NoRule
	    }
	  } else {
	    NoRule
	  }
	}
	
	/**
	 * random boundaries
	 */
	def randomBoundaries(boundProb: Double=0.0) = {
		var seqBoundaries: BitSet = BitSet.empty+0
		var i=1
		while (i<data.length) {
		  if (uboundaries(i) || _random.nextDouble<boundProb)
		    seqBoundaries+=i
		  i+=1
		}
		seqBoundaries+=i
		seqBoundaries
	}
	
	/**
	 * take current segmentation and feed prepare data-structures
	 * for the logistic regression model
	 */
	def updateDel1Model = {
	  println("being update")
	  val relWords = words.filter(x => x._2._1.lastSeg==DROPSEG1 && x._2._1.length>1)
	  println("Size relWords: "+relWords.size)
	  val inputs =  relWords.toList.map(f => ((f._2._1,
	  							  	   getUnderlyingNeighbourAt(f._1+f._2._2.length)))).toArray
      println("built inputs (size="+inputs.length+")")
	  if (inputs.length!=0) {
		  val outputs = relWords.map(f => (if (f._2._1!=f._2._2) 1.0 else 0.0)).toArray
		  println("built outputs (size="+outputs.size+")")
	      delModel1.setInputs(inputs)
	      delModel1.setOutputs(outputs)
		  println("optimize")
		  delModel1.mapLBFGS()
		  println("end update ("+delModel1.weights+")")	      
	  }
	}
	
	/**
	 * build the StartPos --> (Underlying, Surface) HashMap from scratch
	 */
	def buildWords = {
	  words.clear
	  var startPos = 0
	  var curPos = 1
	  while (curPos<data.length) {
	    if (boundaries(curPos)) {
	      words(startPos)=getWord(startPos, curPos)
	      startPos=curPos
	    }
	    curPos+=1
	  }
	  words(startPos)=getWord(startPos, curPos)
	}
	
	def getUnderlyingWordAt(sPos: Int) = words(sPos)._1
	
	// get the neighbour at position nPos; if nPos is uboundary, neighbour is boundaryword
	def getUnderlyingNeighbourAt(nPos: Int) = if (uboundaries(nPos)) UBOUNDARYWORD else words(nPos)._1
	
	/**
	 * return the context of that boundary at assume that the words
	 * have been removed and will be reinserted
	 */
	def removeBoundary(bPos: Int): AContext = {
	  val w1Pos = boundaryToLeft(bPos-1)
	  val leftPos = if (uboundaries(w1Pos)) w1Pos else boundaryToLeft(w1Pos-1)
	  val leftU = getUnderlyingNeighbourAt(leftPos)
	  val w2Pos = bPos
	  val (w1U,w1O,w1D1,w1D2) = getWordWithVar(w1Pos, w2Pos)
	  if (uboundaries(w2Pos)) { //final context
		  del1s-=bPos
		  del2s-=bPos	    
		  FinalContext(leftU,w1U,w1O,w1D1,w1D2,w1Pos)
	  } else {
		  val w2End = boundaryToRight(bPos+1)
		  val (w1U,w1O,w1D1,w1D2) = getWordWithVar(w1Pos, w2Pos)
		  val (w2U,w2O) = getWord(w2Pos,w2End)
		  val (w12U,w12O) = getWord(w1Pos,w2End)
		  val rU = getUnderlyingNeighbourAt(w2End)
		  words.remove(w1Pos)
		  if (boundaries(w2Pos)) {
		    words.remove(w2Pos)
		  }
		  boundaries-=bPos
		  del1s-=bPos
		  del2s-=bPos
		  MedialContext(leftU,w1U,w1O,w1D1,w1D2,w2U,w2O,w12U,w12O,rU,w1Pos)  
	  }
	}
	
	def insertWord(startPos: Int,wUO: (WordType,WordType)) = {
	  words(startPos)=wUO
	  val bPos = startPos+wUO._2.length
	  boundaries+=(bPos)
	  if (wUO._1.finalSeg==DROPSEG1)
	    del1s+=bPos
	  else if (wUO._1.finalSeg==DROPSEG2)
	    del2s+=bPos
	}
	
	def delModelProb: Double = delModel1.loglikelihood() 

	
	/**
	 * the noise function
	 * 
	 * P(s|u), but read: probability of realizing u as s, hence the order
	 * 
	 * precondition: len(u)>1
	 */
	def R(u: WordType, s: WordType, rWord: WordType): Double = {
	  if (u.size==1) {
	    assert(u==s)
	    1.0
	  } else {
	    val res = u.lastSeg match {
	      case DROPSEG1 =>
	          if (u==s)
	            (1-delModel1.prob(features(u,rWord)))
	         else if (u.allButLast==s)
	            delModel1.prob(features(u,rWord))
	         else
	            0.0
	      case DROPSEG2 =>
	        if (u==s)
	          (1-delModel2.prob(features(u,rWord)))
	        else if (u.allButLast==s)
	          delModel2.prob(features(u,rWord))
	        else
	          0.0
	      case _ =>
	        if (u==s)
	        	1.0
	        else
	        	0.0
	    }
	  res
	  }
	}
	
	
	
	/**
	 * find the boundary
	 */
	def _findBoundary(op: Int=>Int)(cPos: Int): Int = { //println(cPos)
	  if (boundaries(cPos))  cPos
	  else _findBoundary(op)(op(cPos))}

   

	def boundaryToLeft: (Int=>Int) = _findBoundary(_-1)
	def boundaryToRight: (Int=>Int) = _findBoundary(_+1)
	
	/**
	 * returns (underlying,observed)
	 */
	def getWord(sPos: Int, ePos: Int): (WordType, WordType) = 
		((new WordType(data, sPos, ePos, { if (del1s(ePos))
	    		 							  DROPSEG1
	    		 							else if (del2s(ePos))
	    		 							  DROPSEG2
	    		 							else
	    		 							  -1}),		 
	      new WordType(data, sPos, ePos, -1))
		)													    
	
	
	/**
	 * returns a 4-tuple (underlying,observed,withDrop1,withDrop2)
	 */
	def getWordWithVar(sPos: Int, ePos: Int): (WordType,WordType,WordType,WordType) = {
	  val (wordU: WordType,wordO: WordType) = getWord(sPos, ePos)
	  val wD1 = new WordType(data,sPos,ePos,DROPSEG1)
	  val wD2 = new WordType(data,sPos,ePos,DROPSEG2)
	  (wordU,wordO,wD1,wD2)
	}
	  



	def getAnalysis(segSep: String, wordSeg: String): String = {
	  def inner(sPos: Int,cPos: Int,res: StringBuilder): String = 
	    if (cPos>=boundaries.size)
	      res.toString
	    else  
	      boundaries(cPos) match {
	      	case false => inner(sPos,cPos+1,res)
	      	case true =>
	      	  val sep = if (uboundaries(cPos)) "\n" else wordSeg
	      	  if (del1s(cPos)) {
	      	      res.append(wToS(new WordType(data,sPos-1,cPos,DROPSEG1),segSep)+sep)
	      	      inner(cPos+1,cPos+1,res)
	      	  } else if (del2s(cPos)) {
	      	      res.append(wToS(new WordType(data,sPos-1, cPos,DROPSEG2),segSep)+sep)
	      	      inner(cPos+1,cPos+1,res)	      	    
	      	  } else {
	      	      res.append(wToS(new WordType(data,sPos-1,cPos,-1),segSep)+sep)
	      	      inner(cPos+1,cPos+1,res)
	      	  }
	      }
	  inner(1,1,new StringBuilder)
	}
	
	def printAnalysis(out: PrintStream = System.out,segSep: String="",wordSeg: String=" ") = {
		out.print(getAnalysis(segSep, wordSeg))
	}
	
	/**
	 * we terminate every utterance with an additional boundary
	 */
	def printAnalysisB(out: PrintStream = System.out,sep: String=" ") = {
	  def inner(cPos: Int): Unit = 
	    if (cPos>=boundaries.size)
	      Unit
	    else  {
	      boundaries(cPos) match { 
	      	case false => 
	      	  out.print("0")
	      	case true =>
	      	  if (del1s(cPos))
	      	    out.print("t")
	      	  else if (del2s(cPos))
	      	    out.print("d")
	      	  else
	      	    out.print("1")
	      	  if (uboundaries(cPos))
	      	    out.print("\n")
	      }
	      inner(cPos+1)
	    }
	    inner(1)  	
	}	
	
	def evaluate: Result = {
		var totalBoundaries = 0;	//boundaries the learner predicts
		var trueBoundaries = 0;		//boundaries in the gold
		var correctBoundaries = 0;	//boundaries the learner gets correct
		var totalTokens = 0;		//tokens the learner predicts
		var trueTokens = 0;			//tokens in the gold
		var correctTokens = 0;		//tokens the learner gets correct
		var totalDrops = 0;
		var trueDrops = 0;
		var correctDrops = 0;
		var trueStartPos=0
		var predStartPos=0		
		for (i <- 1 to nBoundaries) {
		  if (del1s(i))
  	        totalDrops+=1
		  if (golddel1s(i))
  	        trueDrops+=1

		  correctDrops += { if ((golddel1s(i)==del1s(i) && golddel1s(i))) 1 else 0}
		  (boundaries(i),uboundaries(i)) match {
		    case (false,_) => {
		      goldboundaries(i) match {
		        case true =>
		          trueBoundaries+=1
		          trueTokens+=1
		          trueStartPos=i
		          trueDrops+=1          
		        case _ =>
		      }
		    }
		    case (true,false) => {
		      totalBoundaries+=1
		      totalTokens+=1
		      goldboundaries(i) match {
		        case true => {
		          trueBoundaries+=1
		          correctBoundaries+=1
		          trueTokens+=1
		          if (predStartPos==trueStartPos) correctTokens+=1		          
		          trueStartPos=i
		        }
		        case _ =>
		      }
		      predStartPos=i
		    }
		    case (true,true) => {
		      totalTokens+=1
		      trueTokens+=1
		      if (predStartPos==trueStartPos) correctTokens+=1		      
		      predStartPos=i
		      trueStartPos=i
		    }
		  }
		}
		new Result(correctTokens.toFloat/totalTokens,correctTokens.toFloat/trueTokens,
		           correctBoundaries.toFloat/totalBoundaries,correctBoundaries.toFloat/trueBoundaries,
		           correctDrops.toFloat/totalDrops,correctDrops.toFloat/trueDrops,0,0)
	}
	
/*	def showDropProbs: String = delModeLType match {
	  case GlobalFix =>
	    dropProb(null, 1).toString
	  case CVPFollows => ""
/*	    "C "+dropProbRContext(isConsonant.example)+" V "+dropProbRContext(isVowel.example) +
	    " P "+dropProbRContext(isPause.example)*/
	  case CVPLeftRight => ""
/*	    " C_C "+dropProbContext(isConsonant.example,isConsonant.example)+
	    " C_V "+dropProbContext(isConsonant.example,isVowel.example) +
	    " C_P "+dropProbContext(isConsonant.example,isPause.example) +
	    " V_C "+dropProbContext(isVowel.example,isConsonant.example)+
	    " V_V "+dropProbContext(isVowel.example,isVowel.example) +
	    " V_P "+dropProbContext(isVowel.example,isPause.example)*/	    
	}
	*/
}