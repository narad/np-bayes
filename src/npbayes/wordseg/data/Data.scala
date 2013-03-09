package npbayes.wordseg.data

/**
 * we define a simple beta-prior over the application of each variation rule. consequently, we
 * need to track the number of times with which we observed each individual rule applying / not applying
 */

import npbayes.wordseg
import npbayes.distributions.CRP
import java.io._
import scala.collection.mutable.HashMap
import scala.io.Source
import scala.util.Random
import npbayes.wordseg.models.Unigram
import scala.collection.mutable.StringBuilder
import com.google.common.collect.ImmutableList.Builder
import java.util.Arrays
import org.apache.commons.math3.special.Gamma
import npbayes.wordseg.IGNOREDROP
import npbayes.wordseg.FIXDROP
import npbayes.wordseg.INFERDROP
import scala.collection.mutable.ArrayBuffer



abstract class DeletionModel
case object GlobalFix extends DeletionModel
case object CVPFollows extends DeletionModel
case object CVPLeftRight extends DeletionModel

abstract class Boundary
case object NoBoundary extends Boundary
case object WBoundaryDrop1 extends Boundary
case object WBoundaryNoDrop1 extends Boundary
case object WBoundaryDrop2 extends Boundary
case object WBoundaryNoDrop2 extends Boundary
case object WBoundaryNoRule extends Boundary
case object UBoundaryDrop1 extends Boundary
case object UBoundaryNoDrop1 extends Boundary
case object UBoundaryDrop2 extends Boundary
case object UBoundaryNoDrop2 extends Boundary
case object UBoundaryNoRule extends Boundary



abstract class RuleContext

case class SimpleRContext(rC: CoarseType) extends RuleContext
case class SimpleLRContext(lC: CoarseType,rC: CoarseType) extends RuleContext

abstract class CoarseType
case object Consonant extends CoarseType
case object Vowel extends CoarseType
case object Pause extends CoarseType

class Identifier(f: String) {
  var itemsT: Set[SegmentType] = Set[SegmentType]()
  for (l <- scala.io.Source.fromFile(f).getLines)
    itemsT = itemsT+SymbolTable(l)
  
  def apply(x: SegmentType): Boolean =
    itemsT.contains(x)
  
  def example: SegmentType =
    itemsT.head
}









/**
 * variable data --- allows for dropping a single segment at the end of a word
 * keeps boundary information and provides word-extraction functionality
 */
class VarData(fName: String, val dropProb: Double = 0.0,val MISSING1: String = "*", val DROP1: String = "T", val MISSING2: String="DROPD", val DROP2: String = "D", val delModeLType: DeletionModel = GlobalFix) {
	//betaprior counts
	var nD1 = 0
	var nNotD1 = 0
	var nD2 = 0
	var nNotD2 = 0
	val deleted1: HashMap[RuleContext,Int] = new HashMap
	val realized1: HashMap[RuleContext,Int] = new HashMap
	val deleted2: HashMap[RuleContext,Int] = new HashMap
	val realized2: HashMap[RuleContext,Int] = new HashMap
	
	val sentences: ArrayBuffer[(Int,Int)] = new ArrayBuffer
	/**
	 * (x,y) refers to a sentence spanning the characters from
	 * x to y (exclusive, hence substr(x,y)
	 * it is bounded by the boundaries with index x (to the left, not
	 * affected by resampling) and y, hence use
	 * x+1 as the offset
	 */
	
    val UBOUNDARYSYMBOL="UTTERANCEBOUNDARY"
	val UBOUNDARYWORD=segToWord(SymbolTable(UBOUNDARYSYMBOL))
	val DROPSEG=SymbolTable(DROP1)
	
	def isConsonant = wordseg.wordseg.isConsonant
	def isVowel = wordseg.wordseg.isVowel
	def isPause = wordseg.wordseg.isPause
	
	//val delRuleCounts : HashMap[RuleContext, Int] = new HashMap
/*	def makeSimpleRule (rC: SegmentType): SimpleRContext =
	  if isConsonant(rC)
	  	SimpleRContext(Consonant)*/
	
	def segmentToType(segment: SegmentType): CoarseType =
	  if (isConsonant(segment))
	    Consonant
	  else if (isVowel(segment))
	    Vowel
	  else if (isPause(segment))
	    Pause
	  else
	    throw new Error("Error in segmentToType...")
	
	def addDrop1(lSegment: SegmentType, rSegment: SegmentType) = {
	  nD1+=1
	  wordseg.wordseg.dropInferenceMode match {
	    case INFERDROP(_,_) =>
	    	delModeLType match {
		    case CVPFollows =>
		      deleted1.put(SimpleRContext(segmentToType(rSegment)),
				  		deleted1.getOrElse(SimpleRContext(segmentToType(rSegment)),0)+1)
		    case CVPLeftRight =>
		      deleted1.put(SimpleLRContext(segmentToType(lSegment),segmentToType(rSegment)),
				  		deleted1.getOrElse(SimpleLRContext(segmentToType(lSegment),segmentToType(rSegment)),0)+1)
		    case _ =>			  		
	    	}
	    case _ =>
	  }
	}

	def addNoDrop1(lSegment: SegmentType, rSegment: SegmentType) = {
	  nNotD1 += 1
	  wordseg.wordseg.dropInferenceMode match {
	    case INFERDROP(_,_) =>
	    	delModeLType match {
	    		case CVPFollows =>
	    			realized1.put(SimpleRContext(segmentToType(rSegment)),
	    					realized1.getOrElse(SimpleRContext(segmentToType(rSegment)), 0)+1)
	    		case CVPLeftRight =>
	    			realized1.put(SimpleLRContext(segmentToType(lSegment),segmentToType(rSegment)),
	    					realized1.getOrElse(SimpleLRContext(segmentToType(lSegment),segmentToType(rSegment)),0)+1)
	    		case _ =>
	      
	    	}
	    case _ =>
	  }
	}

	def removeDrop1(lSegment: SegmentType, rSegment: SegmentType) = {
	  nD1 -= 1
	  wordseg.wordseg.dropInferenceMode match {
	    case INFERDROP(_,_) =>
		  delModeLType match {
		    case CVPFollows =>
		      val old = deleted1.get(SimpleRContext(segmentToType(rSegment)))
			  old match {
			    case Some(x) =>
			      if (x-1==0)
			        deleted1.remove(SimpleRContext(segmentToType(rSegment)))
			      else
			        deleted1.put(SimpleRContext(segmentToType(rSegment)),x-1)
			    case None =>
			      throw new Error("In removeDrop("+rSegment+")")
			  }
		    case CVPLeftRight =>
		      val old = deleted1.get(SimpleLRContext(segmentToType(lSegment),segmentToType(rSegment)))
			  old match {
			    case Some(x) =>
			      if (x-1==0)
			        deleted1.remove(SimpleLRContext(segmentToType(lSegment),segmentToType(rSegment)))
			      else
			        deleted1.put(SimpleLRContext(segmentToType(lSegment),segmentToType(rSegment)),x-1)
			    case None =>
			      throw new Error("In removeDrop("+lSegment+","+rSegment+")")
			  	      
		      }
		    case _ =>	      
		  }
	    case _ =>
	  }     
	}
	
	def removeNoDrop1(lSegment: SegmentType, rSegment: SegmentType) = {
	  nNotD1 -= 1
	  wordseg.wordseg.dropInferenceMode match {
	    case INFERDROP(_,_) =>
		  delModeLType match {
		    case CVPFollows =>
		      val old = realized1.get(SimpleRContext(segmentToType(rSegment)))
			  old match {
			    case Some(x) =>
			      if (x-1==0)
			        realized1.remove(SimpleRContext(segmentToType(rSegment)))
			      else
			    	realized1.put(SimpleRContext(segmentToType(rSegment)),x-1)
			    case None =>
			      throw new Error("In removeDrop("+rSegment+")")
			  }
		    case CVPLeftRight =>
		      val old = realized1.get(SimpleLRContext(segmentToType(lSegment),segmentToType(rSegment)))
			  old match {
			    case Some(x) =>
			      if (x-1==0)
			        realized1.remove(SimpleLRContext(segmentToType(lSegment),segmentToType(rSegment)))
			      else
			    	realized1.put(SimpleLRContext(segmentToType(lSegment),segmentToType(rSegment)),x-1)
			    case None =>
			      throw new Error("In removeDrop("+lSegment+","+rSegment+")")
			  }
		    case _ =>	      
		  }
	    case _ =>
	  }
	}	

	/**
	 * takes care of the counts for rule applications
	 * we can recover trivially
	 * if underlying ends in t and differs from observed in length --> dropped t
	 * if underlying ends in t and doesn't differ in length --> realization of t
	 * if underlying ends in d and differs --> dropped d
	 * if underlying endsin d and doesn't differ --> realization of d
	 */
	def removeTransformation(observed: WordType, underlying: WordType,rightContext: WordType) = {
	  val uEndsIn = SymbolTable(underlying.get(underlying.size()-1))
	  val sameLength = observed.size == underlying.size
	  if (underlying.size>1)
		  uEndsIn match {
		    case DROP1 =>
		      if (sameLength)
		        removeNoDrop1(underlying.get(underlying.size()-2), rightContext.get(0))
		      else
		        removeDrop1(underlying.get(underlying.size()-2),rightContext.get(0))
		    case DROP2 =>
		    case _ =>
		  }
	}
	def addTransformation(observed: WordType, underlying: WordType,rightContext: WordType) = {
	  val uEndsIn = SymbolTable(underlying.get(underlying.size()-1))
	  val sameLength = observed.size == underlying.size
	  if (underlying.size>1)
		  uEndsIn match {
		    case DROP1 =>
		      if (sameLength)
		        addNoDrop1(underlying.get(underlying.size()-2), rightContext.get(0))
		      else
		        addDrop1(underlying.get(underlying.size()-2),rightContext.get(0))
		    case DROP2 =>
		    case _ =>
		  }
	}
	
	
	/**
	 * @return	length of the sentence (= number of boundaries)
	 */
	def getSentenceLength(x:(Int, Int)): Int =
	  x._2-x._1
	
	/**
	 * Initialize the data and goldBoundaries
	 */
	val (data,goldBoundaries) = {
		var seqPhones = Vector.empty[Int]
		var seqBoundaries: Vector[Boundary] = Vector.empty:+UBoundaryNoDrop1
		var startPos = 0
		var stringPos = 0
		def processLine(line: String) = {
			for (w <- line.stripLineEnd.split("\t")) {
				for (c: String <- w.split(" ")) {
				seqPhones = seqPhones:+ SymbolTable(c)
					seqBoundaries = seqBoundaries:+NoBoundary
					stringPos+=1
				}
	      // adjust for word-boundaries --- last NoBoundary is in fact a word-boundary
				if (SymbolTable(seqPhones.last)==MISSING1) {
					seqPhones = seqPhones.dropRight(1)
					seqBoundaries = seqBoundaries.dropRight(2):+WBoundaryDrop1
				} else {
					seqBoundaries = seqBoundaries.dropRight(1):+WBoundaryNoDrop1
				}
			}
			seqBoundaries = seqBoundaries.last match {
				case WBoundaryDrop1 => seqBoundaries.dropRight(1):+UBoundaryDrop1
				case WBoundaryNoDrop1 => seqBoundaries.dropRight(1):+UBoundaryNoDrop1
			}
			sentences+=((startPos,stringPos))
			startPos=stringPos
		}
		for (l <- Source.fromFile(fName).getLines) processLine(l)
		val phones = new Builder[Int]
		for (x <- seqPhones)
		  phones.add(x)
		(phones.build,seqBoundaries.toArray)
	}
	

	
	

	val _random = new Random()
	
	var boundaries = randomBoundaries().toArray
	
	/**
	 * randomize boundaries
	 */
	def randomBoundaries(boundProb: Double=0.0) = {
		var seqBoundaries: Vector[Boundary] = Vector.empty
		for (b <- goldBoundaries) {
			b match {
			case UBoundaryDrop1 | UBoundaryNoDrop1 => seqBoundaries=seqBoundaries:+{if (_random.nextDouble<0) UBoundaryDrop1 else UBoundaryNoDrop1}
			case WBoundaryDrop1 | WBoundaryNoDrop1 | NoBoundary => seqBoundaries=seqBoundaries:+{if (_random.nextDouble>boundProb) NoBoundary else if (_random.nextDouble<dropProb) WBoundaryDrop1 else WBoundaryNoDrop1}
			}
		}
		seqBoundaries
	}
	
	/**
	 * provides the interface to the outside, distribute from here
	 */
	def dropProb(s: WordType, rContext: SegmentType): Double = delModeLType match {
	  case GlobalFix => dropProbUniform
  	  case CVPFollows => dropProbRContext(rContext)
	  case CVPLeftRight => dropProbContext(s.get(s.size-2),rContext)	  	    
	}

	def dropProbUniform = npbayes.wordseg.wordseg.dropInferenceMode match {
	  case IGNOREDROP =>
	    throw new Error("Ignoring drop, shouldn't be in dropProbUniform")
	  case FIXDROP(p) =>
	    p
	  case INFERDROP(priorDrop,priorNodrop) =>
	    (nD1+priorDrop) / (priorDrop+priorNodrop+nD1+nNotD1)
	}

		def dropProbRContext(rContext: SegmentType): Double = wordseg.wordseg.dropInferenceMode match {
	  case IGNOREDROP =>
	    throw new Error("Ignoring drop, Shouldn't be in dropProbRContext")
	  case INFERDROP(priorDrop,priorNodrop) =>
		  val dropped = deleted1.getOrElse(SimpleRContext(segmentToType(rContext)), 0)
		  val notDropped = realized1.getOrElse(SimpleRContext(segmentToType(rContext)), 0)
		  (dropped + priorDrop) / (dropped + notDropped + priorDrop + priorNodrop)
	  case FIXDROP(p) =>
	    if (isConsonant(rContext))
		    0.37
		  else if (isVowel(rContext))
		    0.22
		    else
		      0.14
	}
	
	def dropProbContext(lContext: SegmentType, rContext: SegmentType): Double = /*dropProb*/ {
	  wordseg.wordseg.dropInferenceMode match {
	    case IGNOREDROP =>
	      throw new Error("Ignoring drop, Shouldn't be in dropProbRContext")
	    case INFERDROP(priorDrop,priorNodrop) =>
	      val dropped = deleted1.getOrElse(SimpleLRContext(segmentToType(lContext),segmentToType(rContext)), 0)
		  val notDropped = realized1.getOrElse(SimpleLRContext(segmentToType(lContext),segmentToType(rContext)), 0)
		  (dropped + priorDrop) / (dropped + notDropped + priorDrop + priorNodrop)
	    case FIXDROP(p) =>
		  if (isConsonant(rContext))
		    if (isConsonant(lContext))
		    	0.62
		    else
		    	0.23
		  else
		    if (isVowel(rContext))
		      if (isConsonant(lContext))
		    	0.42
		      else
		        0.15
		    else
		      if (isPause(rContext))
		        if (isConsonant(lContext))
		        	0.36
		        else
		          0.07
		      else
		        throw new Error("Unknown Segment "+SymbolTable(rContext))	      
	  }

	}//dropProb	//TODO - word-specific probabilities  */
	
	
	def setBoundary(pos: Int, b: Boundary): Unit = 
	  boundaries(pos)= b
	
	  
	def delModelProb: Double = 
	  if (wordseg.wordseg.dropInferenceMode==IGNOREDROP)
		  0
	  else { 
		  delModeLType match {
		  case GlobalFix =>
		    wordseg.wordseg.dropInferenceMode match {
		      case INFERDROP(priorDrop,priorNodrop) =>
			  	Gamma.logGamma(nD1+priorDrop)+Gamma.logGamma(nNotD1+priorNodrop)-Gamma.logGamma(nD1+nNotD1+priorDrop+priorNodrop)+
			  	Gamma.logGamma(priorDrop+priorNodrop)-Gamma.logGamma(priorDrop)-Gamma.logGamma(priorNodrop)
		      case FIXDROP(p) =>
			  	nD1*math.log(p)+nNotD1*math.log(1-p)
		    }
		  case CVPFollows =>
		    wordseg.wordseg.dropInferenceMode match {
		      case INFERDROP(priorDrop,priorNodrop) =>
			  	Gamma.logGamma(deleted1.getOrElse(SimpleRContext(Consonant),0)+priorDrop)+Gamma.logGamma(realized1.getOrElse(SimpleRContext(Consonant),0)+priorNodrop)-Gamma.logGamma(deleted1.getOrElse(SimpleRContext(Consonant),0)+realized1.getOrElse(SimpleRContext(Consonant),0)+priorDrop+priorNodrop)+
			  	Gamma.logGamma(priorDrop+priorNodrop)-Gamma.logGamma(priorDrop)-Gamma.logGamma(priorNodrop)+	        
			  	Gamma.logGamma(deleted1.getOrElse(SimpleRContext(Vowel),0)+priorDrop)+Gamma.logGamma(realized1.getOrElse(SimpleRContext(Vowel),0)+priorNodrop)-Gamma.logGamma(deleted1.getOrElse(SimpleRContext(Vowel),0)+realized1.getOrElse(SimpleRContext(Vowel),0)+priorDrop+priorNodrop)+
			  	Gamma.logGamma(priorDrop+priorNodrop)-Gamma.logGamma(priorDrop)-Gamma.logGamma(priorNodrop)+
			  	Gamma.logGamma(deleted1.getOrElse(SimpleRContext(Pause),0)+priorDrop)+Gamma.logGamma(realized1.getOrElse(SimpleRContext(Pause),0)+priorNodrop)-Gamma.logGamma(deleted1.getOrElse(SimpleRContext(Pause),0)+realized1.getOrElse(SimpleRContext(Pause),0)+priorDrop+priorNodrop)+
			  	Gamma.logGamma(priorDrop+priorNodrop)-Gamma.logGamma(priorDrop)-Gamma.logGamma(priorNodrop)
			  case FIXDROP(p) =>
			  	deleted1.getOrElse(SimpleRContext(Consonant),0)*math.log(0.37)+realized1.getOrElse(SimpleRContext(Consonant),0)*math.log(1-0.37)+
			  	deleted1.getOrElse(SimpleRContext(Vowel),0)*math.log(0.22) + realized1.getOrElse(SimpleRContext(Vowel),0)*math.log(1-0.22)+
			  	deleted1.getOrElse(SimpleRContext(Pause),0)*math.log(0.14)+realized1.getOrElse(SimpleRContext(Pause),0)*math.log(1-0.14)
		    }
		  case CVPLeftRight =>
		    wordseg.wordseg.dropInferenceMode match {
		      case INFERDROP(priorDrop,priorNodrop) =>
			  	Gamma.logGamma(deleted1.getOrElse(SimpleLRContext(Consonant,Consonant),0)+priorDrop)+Gamma.logGamma(realized1.getOrElse(SimpleLRContext(Consonant,Consonant),0)+priorNodrop)-Gamma.logGamma(deleted1.getOrElse(SimpleLRContext(Consonant,Consonant),0)+realized1.getOrElse(SimpleLRContext(Consonant,Consonant),0)+priorDrop+priorNodrop)+
			  	Gamma.logGamma(priorDrop+priorNodrop)-Gamma.logGamma(priorDrop)-Gamma.logGamma(priorNodrop)+	        
			  	Gamma.logGamma(deleted1.getOrElse(SimpleLRContext(Consonant,Vowel),0)+priorDrop)+Gamma.logGamma(realized1.getOrElse(SimpleLRContext(Consonant,Vowel),0)+priorNodrop)-Gamma.logGamma(deleted1.getOrElse(SimpleLRContext(Consonant,Vowel),0)+realized1.getOrElse(SimpleLRContext(Consonant,Vowel),0)+priorDrop+priorNodrop)+
			  	Gamma.logGamma(priorDrop+priorNodrop)-Gamma.logGamma(priorDrop)-Gamma.logGamma(priorNodrop)+
			  	Gamma.logGamma(deleted1.getOrElse(SimpleLRContext(Consonant,Pause),0)+priorDrop)+Gamma.logGamma(realized1.getOrElse(SimpleLRContext(Consonant,Pause),0)+priorNodrop)-Gamma.logGamma(deleted1.getOrElse(SimpleLRContext(Consonant,Pause),0)+realized1.getOrElse(SimpleLRContext(Consonant,Pause),0)+priorDrop+priorNodrop)+
			  	Gamma.logGamma(priorDrop+priorNodrop)-Gamma.logGamma(priorDrop)-Gamma.logGamma(priorNodrop)+
			  	Gamma.logGamma(deleted1.getOrElse(SimpleLRContext(Vowel,Consonant),0)+priorDrop)+Gamma.logGamma(realized1.getOrElse(SimpleLRContext(Vowel,Consonant),0)+priorNodrop)-Gamma.logGamma(deleted1.getOrElse(SimpleLRContext(Vowel,Consonant),0)+realized1.getOrElse(SimpleLRContext(Vowel,Consonant),0)+priorDrop+priorNodrop)+
			  	Gamma.logGamma(priorDrop+priorNodrop)-Gamma.logGamma(priorDrop)-Gamma.logGamma(priorNodrop)+	        
			  	Gamma.logGamma(deleted1.getOrElse(SimpleLRContext(Vowel,Vowel),0)+priorDrop)+Gamma.logGamma(realized1.getOrElse(SimpleLRContext(Vowel,Vowel),0)+priorNodrop)-Gamma.logGamma(deleted1.getOrElse(SimpleLRContext(Vowel,Vowel),0)+realized1.getOrElse(SimpleLRContext(Vowel,Vowel),0)+priorDrop+priorNodrop)+
			  	Gamma.logGamma(priorDrop+priorNodrop)-Gamma.logGamma(priorDrop)-Gamma.logGamma(priorNodrop)+
			  	Gamma.logGamma(deleted1.getOrElse(SimpleLRContext(Vowel,Pause),0)+priorDrop)+Gamma.logGamma(realized1.getOrElse(SimpleLRContext(Vowel,Pause),0)+priorNodrop)-Gamma.logGamma(deleted1.getOrElse(SimpleLRContext(Vowel,Pause),0)+realized1.getOrElse(SimpleLRContext(Vowel,Pause),0)+priorDrop+priorNodrop)+
			  	Gamma.logGamma(priorDrop+priorNodrop)-Gamma.logGamma(priorDrop)-Gamma.logGamma(priorNodrop)
		     case FIXDROP(_) =>
		        deleted1.getOrElse(SimpleLRContext(Consonant,Consonant),0)*math.log(0.62)+realized1.getOrElse(SimpleLRContext(Consonant,Consonant),0)*math.log(1-0.62)+     
			  	deleted1.getOrElse(SimpleLRContext(Consonant,Vowel),0)*math.log(0.42)+realized1.getOrElse(SimpleLRContext(Consonant,Vowel),0)*math.log(1-0.42)+
			  	deleted1.getOrElse(SimpleLRContext(Consonant,Pause),0)*math.log(0.36)+realized1.getOrElse(SimpleLRContext(Consonant,Pause),0)*math.log(1-0.36)+
			  	deleted1.getOrElse(SimpleLRContext(Vowel,Consonant),0)*math.log(0.23)+realized1.getOrElse(SimpleLRContext(Vowel,Consonant),0)*math.log(1-0.23)+
			  	deleted1.getOrElse(SimpleLRContext(Vowel,Vowel),0)*math.log(0.15)+realized1.getOrElse(SimpleLRContext(Vowel,Vowel),0)*math.log(1-0.15)+
			  	deleted1.getOrElse(SimpleLRContext(Vowel,Pause),0)*math.log(0.07)+realized1.getOrElse(SimpleLRContext(Vowel,Pause),0)*math.log(1-0.07)
			  
		    }
	  }
	} 

	
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
	  val res = SymbolTable(u.get(u.size-1)) match {
	  case DROP1 =>
	    val rContext = rWord.get(rWord.size()-1)
	    if (u==s)
	      (1-dropProb(u,rContext))
	    else
	      if (u.subList(0,u.size-1)==s)
	        dropProb(u,rContext)
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
	
	
	
	
	def _findBoundary(op: Int=>Int)(cPos: Int): Int = boundaries(cPos) match {
    	case WBoundaryDrop1 | WBoundaryNoDrop1 | UBoundaryDrop1 | UBoundaryNoDrop1 => cPos
    	case NoBoundary =>  _findBoundary(op)(op(cPos))
	}
   

	def boundaryToLeft: (Int=>Int) = _findBoundary(_-1)
	def boundaryToRight: (Int=>Int) = _findBoundary(_+1)
	
	def boundaryToRightSentence(x: (Int,Int)): (Int=>Int) = _findBoundary(_+1+x._1)

	def getWordWithVarSentence(sPos: Int, ePos: Int,s: (Int,Int)): (WordType,WordType,WordType) = {
	  val offset = s._1+1
	  getWordWithVar(offset+sPos,offset+ePos)
	}

	
	/**
	 * returns a triple (observed,underlying,withDrop)
	 */
	def getWordWithVar(sPos: Int, ePos: Int): (WordType,WordType,WordType) = {
	  val word = data.subList(sPos, ePos)
	  val wD = suffix(word,DROPSEG)
	  boundaries(ePos) match {
	    case UBoundaryDrop1 | WBoundaryDrop1 =>
	      (word,wD,wD)
	    case _ =>
	      (word,word,wD)
	  }
	}
	
	
	def getWordSentence(sPos: Int, ePos: Int,s: (Int,Int)): (WordType,WordType) = {
	  val offset = s._1+1
	  getWord(offset+sPos,offset+ePos)
	}
	  
	
	/**
	 * returns a tuple (observed,underlying)
	 * 
	 * this may save the additional cost of creating a copy of the token with an additional dropsegment
	 */
	def getWord(sPos: Int, ePos: Int): (WordType,WordType) = {
	  val word = data.subList(sPos, ePos)
	  boundaries(ePos) match {
	    case UBoundaryDrop1 | WBoundaryDrop1 =>
	      (word,suffix(word,DROPSEG))
	    case _ =>
	      (word,word)
	  }
	}


	def getAnalysis: String = {
	  def inner(sPos: Int,cPos: Int,res: StringBuilder): String = 
	    if (cPos>=boundaries.size)
	      res.toString
	    else 
	      boundaries(cPos) match {
	      	case NoBoundary => inner(sPos,cPos+1,res)
	      	case WBoundaryDrop1 => {
 	      	  res.append(wToS(suffix(data.subList(sPos-1, cPos),DROPSEG))+"::")
 	      	  inner(cPos+1,cPos+1,res)
	      	}
	      	case WBoundaryNoDrop1 => {
 	      	  res.append(wToS(data.subList(sPos-1, cPos))+"::")
 	      	  inner(cPos+1,cPos+1,res)
	      	}
	      	case UBoundaryDrop1 => {
	      	  res.append(wToS(suffix(data.subList(sPos-1, cPos),DROPSEG))+"\n")
 	      	  inner(cPos+1,cPos+1,res)
	      	}
	      	case UBoundaryNoDrop1 => {
 	      	  res.append(wToS(data.subList(sPos-1, cPos))+"\n")
 	      	  inner(cPos+1,cPos+1,res)
	      	}
	    }
	  inner(1,1,new StringBuilder)
	}
	
	def printAnalysis(out: PrintStream = System.out,sep: String=" ") = {
	  def inner(sPos: Int,cPos: Int): Unit = 
	    if (cPos>=boundaries.size)
	      Unit
	    else 
	      boundaries(cPos) match {
	      	case NoBoundary => inner(sPos,cPos+1)
	      	case WBoundaryDrop1 => {
 	      	  out.print(
 	      	      wToS(suffix(data.subList(sPos-1, cPos),DROPSEG))+sep)
 	      	  inner(cPos+1,cPos+1)
	      	}
	      	case WBoundaryNoDrop1 => {
 	      	  out.print(wToS(data.subList(sPos-1, cPos))+sep)
 	      	  inner(cPos+1,cPos+1)
	      	}
	      	case UBoundaryDrop1 => {
	      	  out.print(wToS(suffix(data.subList(sPos-1, cPos),DROPSEG))+"\n")
 	      	  inner(cPos+1,cPos+1)
	      	}
	      	case UBoundaryNoDrop1 => {
 	      	  out.print(wToS(data.subList(sPos-1, cPos))+"\n")
 	      	  inner(cPos+1,cPos+1)
	      	}
	    }
	  inner(1,1)
	}
	
	def printAnalysisB(out: PrintStream = System.out,sep: String=" ") = {
	  def inner(sPos: Int,cPos: Int): Unit = 
	    if (cPos>=boundaries.size)
	      Unit
	    else 
	      boundaries(cPos) match {
	      	case NoBoundary => {
	      	  out.print("0")
	      	  inner(sPos,cPos+1)
	      	}
	      	case WBoundaryDrop1 => {
 	      	  out.print("t")
 	      	  inner(cPos+1,cPos+1)
	      	}
	      	case WBoundaryNoDrop1 => {
 	      	  out.print("1")
 	      	  inner(cPos+1,cPos+1)
	      	}
	      	case UBoundaryDrop1 => {
	      	  out.print("t\n")
 	      	  inner(cPos+1,cPos+1)
	      	}
	      	case UBoundaryNoDrop1 => {
	      	  out.print("\n")
 	      	  inner(cPos+1,cPos+1)
	      	}
	    }
	  inner(1,1)
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
//		HashMap<ImmutableList<Short>,Integer> proposedLexicon = new HashMap<ImmutableList<Short>,Integer>();	//words in the proposed segmentation
//		ashMap<ImmutableList<Short>,Integer> trueLexicon = new HashMap<ImmutableList<Short>, Integer>();		//words in the true segmentation
		var trueStartPos=0
		var predStartPos=0		
		for (i <- 1 to boundaries.size-1) {
		  boundaries(i) match {
		    case NoBoundary => {
		      goldBoundaries(i) match {
		        case WBoundaryDrop1 =>
		          trueBoundaries+=1
		          trueTokens+=1
		          trueStartPos=i
		          trueDrops+=1
		        case WBoundaryNoDrop1 =>
		          trueBoundaries+=1
		          trueTokens+=1
		          trueStartPos=i		          
		        case _ =>
		      }
		    }
		    case WBoundaryDrop1 => {
		      totalBoundaries+=1
		      totalTokens+=1
		      totalDrops+=1
		      goldBoundaries(i) match {
		        case WBoundaryDrop1 | WBoundaryNoDrop1 => {
		          trueBoundaries+=1
		          correctBoundaries+=1
		          trueTokens+=1
		          // don't punish for wrong t-postulation
		          //if (predStartPos==trueStartPos && goldBoundaries(i)==boundaries(i)) correctTokens+=1
		          if (predStartPos==trueStartPos) correctTokens+=1		          
		          trueStartPos=i
		          if (goldBoundaries(i)==WBoundaryDrop1) {
		            trueDrops+=1
		            correctDrops+=1
		          }
		        }
		        case _ =>
		      }
		      predStartPos=i
		    }
		    case WBoundaryNoDrop1 => {
		      totalBoundaries+=1
		      totalTokens+=1
		      goldBoundaries(i) match {
		        case WBoundaryDrop1 | WBoundaryNoDrop1 => {
		          trueBoundaries+=1
		          correctBoundaries+=1
		          trueTokens+=1
		          if (predStartPos==trueStartPos) correctTokens+=1
		          trueStartPos=i
		          if (goldBoundaries(i)==WBoundaryDrop1) {
		            trueDrops+=1
		          }		          
		        }
		        case _ =>
		      }
		      predStartPos=i
		    }
		    case UBoundaryDrop1 | UBoundaryNoDrop1 => {
		      totalTokens+=1
		      trueTokens+=1
		      if (boundaries(i)==UBoundaryDrop1) totalDrops+=1
		      if (goldBoundaries(i)==UBoundaryDrop1) {
		        trueDrops+=1
		        correctDrops+= {if (goldBoundaries(i)==boundaries(i)) 1 else 0}
		      }
		      //don't punish for wrong t-postulation
		      //if (predStartPos==trueStartPos  && goldBoundaries(i)==boundaries(i)) correctTokens+=1
		      if (predStartPos==trueStartPos) correctTokens+=1		      
		      predStartPos=i
		      trueStartPos=i
		    }
		  }
		}
		new Result(correctTokens.toFloat/totalTokens,correctTokens.toFloat/trueTokens,
		           correctBoundaries.toFloat/totalBoundaries,correctBoundaries.toFloat/trueBoundaries,
		           correctDrops.toFloat/totalDrops,correctDrops.toFloat/trueDrops)
	}
	
	def showDropProbs: String = delModeLType match {
	  case GlobalFix =>
	    dropProb(null, 1).toString
	  case CVPFollows => 
	    "C "+dropProbRContext(isConsonant.example)+" V "+dropProbRContext(isVowel.example) +
	    " P "+dropProbRContext(isPause.example)
	  case CVPLeftRight => 
	    " C_C "+dropProbContext(isConsonant.example,isConsonant.example)+
	    " C_V "+dropProbContext(isConsonant.example,isVowel.example) +
	    " C_P "+dropProbContext(isConsonant.example,isPause.example) +
	    " V_C "+dropProbContext(isVowel.example,isConsonant.example)+
	    " V_V "+dropProbContext(isVowel.example,isVowel.example) +
	    " V_P "+dropProbContext(isVowel.example,isPause.example)	    
	}
	
}