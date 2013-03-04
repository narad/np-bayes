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
import npbayes.wordseg.IGNOREDROP
import npbayes.wordseg.FIXDROP
import npbayes.wordseg.IGNOREDROP
import npbayes.wordseg.FIXDROP
import npbayes.wordseg.INFERDROP
import npbayes.wordseg.IGNOREDROP
import npbayes.wordseg.INFERDROP
import npbayes.wordseg.FIXDROP
import npbayes.wordseg.INFERDROP
import npbayes.wordseg.FIXDROP
import npbayes.wordseg.IGNOREDROP
import npbayes.wordseg.IGNOREDROP
import npbayes.wordseg.IGNOREDROP
import npbayes.wordseg.INFERDROP



abstract class DeletionModel
case object GlobalFix extends DeletionModel
case object CVPFollows extends DeletionModel
case object CVPLeftRight extends DeletionModel

abstract class Boundary
case object NoBoundary extends Boundary
case object WBoundaryDrop extends Boundary
case object WBoundaryNodrop extends Boundary
case object UBoundaryDrop extends Boundary
case object UBoundaryNodrop extends Boundary

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
    itemsT.first
}









/**
 * variable data --- allows for dropping a single segment at the end of a word
 * keeps boundary information and provides word-extraction functionality
 */
class VarData(fName: String, val dropProb: Double = 0.0,val MISSING: String = "*", val DROPSYMBOL: String = "T", val delModeLType: DeletionModel = GlobalFix) {
	//betaprior counts
	var nDropped = 0
	var nNotDropped = 0
	val deleted: HashMap[RuleContext,Int] = new HashMap
	val realized: HashMap[RuleContext,Int] = new HashMap
	
	
    val UBOUNDARYSYMBOL="UTTERANCEBOUNDARY"
	val UBOUNDARYWORD=segToWord(SymbolTable(UBOUNDARYSYMBOL))
	val DROPSEG=SymbolTable(DROPSYMBOL)
	
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
	
	def addDrop(lSegment: SegmentType, rSegment: SegmentType) = {
	  nDropped+=1
	  wordseg.wordseg.dropInferenceMode match {
	    case INFERDROP(_,_) =>
	    	delModeLType match {
		    case CVPFollows =>
		      deleted.put(SimpleRContext(segmentToType(rSegment)),
				  		deleted.getOrElse(SimpleRContext(segmentToType(rSegment)),0)+1)
		    case CVPLeftRight =>
		      deleted.put(SimpleLRContext(segmentToType(lSegment),segmentToType(rSegment)),
				  		deleted.getOrElse(SimpleLRContext(segmentToType(lSegment),segmentToType(rSegment)),0)+1)
		    case _ =>			  		
	    	}
	    case _ =>
	  }
	}

	def addNodrop(lSegment: SegmentType, rSegment: SegmentType) = {
	  nNotDropped += 1
	  wordseg.wordseg.dropInferenceMode match {
	    case INFERDROP(_,_) =>
	    	delModeLType match {
	    		case CVPFollows =>
	    			realized.put(SimpleRContext(segmentToType(rSegment)),
	    					realized.getOrElse(SimpleRContext(segmentToType(rSegment)), 0)+1)
	    		case CVPLeftRight =>
	    			realized.put(SimpleLRContext(segmentToType(lSegment),segmentToType(rSegment)),
	    					realized.getOrElse(SimpleLRContext(segmentToType(lSegment),segmentToType(rSegment)),0)+1)
	    		case _ =>
	      
	    	}
	    case _ =>
	  }
	}

	def removeDrop(lSegment: SegmentType, rSegment: SegmentType) = {
	  nDropped -= 1
	  wordseg.wordseg.dropInferenceMode match {
	    case INFERDROP(_,_) =>
		  delModeLType match {
		    case CVPFollows =>
		      val old = deleted.get(SimpleRContext(segmentToType(rSegment)))
			  old match {
			    case Some(x) =>
			      if (x-1==0)
			        deleted.remove(SimpleRContext(segmentToType(rSegment)))
			      else
			        deleted.put(SimpleRContext(segmentToType(rSegment)),x-1)
			    case None =>
			      throw new Error("In removeDrop("+rSegment+")")
			  }
		    case CVPLeftRight =>
		      val old = deleted.get(SimpleLRContext(segmentToType(lSegment),segmentToType(rSegment)))
			  old match {
			    case Some(x) =>
			      if (x-1==0)
			        deleted.remove(SimpleLRContext(segmentToType(lSegment),segmentToType(rSegment)))
			      else
			        deleted.put(SimpleLRContext(segmentToType(lSegment),segmentToType(rSegment)),x-1)
			    case None =>
			      throw new Error("In removeDrop("+lSegment+","+rSegment+")")
			  	      
		      }
		    case _ =>	      
		  }
	    case _ =>
	  }     
	}
	
	def removeNodrop(lSegment: SegmentType, rSegment: SegmentType) = {
	  nNotDropped -= 1
	  wordseg.wordseg.dropInferenceMode match {
	    case INFERDROP(_,_) =>
		  delModeLType match {
		    case CVPFollows =>
		      val old = realized.get(SimpleRContext(segmentToType(rSegment)))
			  old match {
			    case Some(x) =>
			      if (x-1==0)
			        realized.remove(SimpleRContext(segmentToType(rSegment)))
			      else
			    	realized.put(SimpleRContext(segmentToType(rSegment)),x-1)
			    case None =>
			      throw new Error("In removeDrop("+rSegment+")")
			  }
		    case CVPLeftRight =>
		      val old = realized.get(SimpleLRContext(segmentToType(lSegment),segmentToType(rSegment)))
			  old match {
			    case Some(x) =>
			      if (x-1==0)
			        realized.remove(SimpleLRContext(segmentToType(lSegment),segmentToType(rSegment)))
			      else
			    	realized.put(SimpleLRContext(segmentToType(lSegment),segmentToType(rSegment)),x-1)
			    case None =>
			      throw new Error("In removeDrop("+lSegment+","+rSegment+")")
			  }
		    case _ =>	      
		  }
	    case _ =>
	  }
	}	
	  	  
	
	/**
	 * Initialize the data and goldBoundaries
	 */
	val (data: WordType,goldBoundaries: Array[Boundary]) = {
		var seqPhones = Vector.empty[Int]
		var seqBoundaries: Vector[Boundary] = Vector.empty:+UBoundaryNodrop
		def processLine(line: String) = {
			for (w <- line.stripLineEnd.split("\t")) {
				for (c: String <- w.split(" ")) {
				seqPhones = seqPhones:+ SymbolTable(c)
					seqBoundaries = seqBoundaries:+NoBoundary
				}
	      // adjust for word-boundaries --- last NoBoundary is in fact a word-boundary
				if (SymbolTable(seqPhones.last)==MISSING) {
					seqPhones = seqPhones.dropRight(1)
					seqBoundaries = seqBoundaries.dropRight(2):+WBoundaryDrop
				} else {
					seqBoundaries = seqBoundaries.dropRight(1):+WBoundaryNodrop
				}
			}
			seqBoundaries = seqBoundaries.last match {
				case WBoundaryDrop => seqBoundaries.dropRight(1):+UBoundaryDrop
				case WBoundaryNodrop => seqBoundaries.dropRight(1):+UBoundaryNodrop
			}	    
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
			case UBoundaryDrop | UBoundaryNodrop => seqBoundaries=seqBoundaries:+{if (_random.nextDouble<0) UBoundaryDrop else UBoundaryNodrop}
			case WBoundaryDrop | WBoundaryNodrop | NoBoundary => seqBoundaries=seqBoundaries:+{if (_random.nextDouble>boundProb) NoBoundary else if (_random.nextDouble<dropProb) WBoundaryDrop else WBoundaryNodrop}
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
	    (nDropped+priorDrop) / (priorDrop+priorNodrop+nDropped+nNotDropped)
	}

		def dropProbRContext(rContext: SegmentType): Double = wordseg.wordseg.dropInferenceMode match {
	  case IGNOREDROP =>
	    throw new Error("Ignoring drop, Shouldn't be in dropProbRContext")
	  case INFERDROP(priorDrop,priorNodrop) =>
		  val dropped = deleted.getOrElse(SimpleRContext(segmentToType(rContext)), 0)
		  val notDropped = realized.getOrElse(SimpleRContext(segmentToType(rContext)), 0)
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
	      val dropped = deleted.getOrElse(SimpleLRContext(segmentToType(lContext),segmentToType(rContext)), 0)
		  val notDropped = realized.getOrElse(SimpleLRContext(segmentToType(lContext),segmentToType(rContext)), 0)
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
			  	Gamma.logGamma(nDropped+priorDrop)+Gamma.logGamma(nNotDropped+priorNodrop)-Gamma.logGamma(nDropped+nNotDropped+priorDrop+priorNodrop)+
			  	Gamma.logGamma(priorDrop+priorNodrop)-Gamma.logGamma(priorDrop)-Gamma.logGamma(priorNodrop)
		      case FIXDROP(p) =>
			  	nDropped*math.log(p)+nNotDropped*math.log(1-p)
		    }
		  case CVPFollows =>
		    wordseg.wordseg.dropInferenceMode match {
		      case INFERDROP(priorDrop,priorNodrop) =>
			  	Gamma.logGamma(deleted.getOrElse(SimpleRContext(Consonant),0)+priorDrop)+Gamma.logGamma(realized.getOrElse(SimpleRContext(Consonant),0)+priorNodrop)-Gamma.logGamma(deleted.getOrElse(SimpleRContext(Consonant),0)+realized.getOrElse(SimpleRContext(Consonant),0)+priorDrop+priorNodrop)+
			  	Gamma.logGamma(priorDrop+priorNodrop)-Gamma.logGamma(priorDrop)-Gamma.logGamma(priorNodrop)+	        
			  	Gamma.logGamma(deleted.getOrElse(SimpleRContext(Vowel),0)+priorDrop)+Gamma.logGamma(realized.getOrElse(SimpleRContext(Vowel),0)+priorNodrop)-Gamma.logGamma(deleted.getOrElse(SimpleRContext(Vowel),0)+realized.getOrElse(SimpleRContext(Vowel),0)+priorDrop+priorNodrop)+
			  	Gamma.logGamma(priorDrop+priorNodrop)-Gamma.logGamma(priorDrop)-Gamma.logGamma(priorNodrop)+
			  	Gamma.logGamma(deleted.getOrElse(SimpleRContext(Pause),0)+priorDrop)+Gamma.logGamma(realized.getOrElse(SimpleRContext(Pause),0)+priorNodrop)-Gamma.logGamma(deleted.getOrElse(SimpleRContext(Pause),0)+realized.getOrElse(SimpleRContext(Pause),0)+priorDrop+priorNodrop)+
			  	Gamma.logGamma(priorDrop+priorNodrop)-Gamma.logGamma(priorDrop)-Gamma.logGamma(priorNodrop)
			  case FIXDROP(p) =>
			  	deleted.getOrElse(SimpleRContext(Consonant),0)*math.log(0.37)+realized.getOrElse(SimpleRContext(Consonant),0)*math.log(1-0.37)+
			  	deleted.getOrElse(SimpleRContext(Vowel),0)*math.log(0.22) + realized.getOrElse(SimpleRContext(Vowel),0)*math.log(1-0.22)+
			  	deleted.getOrElse(SimpleRContext(Pause),0)*math.log(0.14)+realized.getOrElse(SimpleRContext(Pause),0)*math.log(1-0.14)
		    }
		  case CVPLeftRight =>
		    wordseg.wordseg.dropInferenceMode match {
		      case INFERDROP(priorDrop,priorNodrop) =>
			  	Gamma.logGamma(deleted.getOrElse(SimpleLRContext(Consonant,Consonant),0)+priorDrop)+Gamma.logGamma(realized.getOrElse(SimpleLRContext(Consonant,Consonant),0)+priorNodrop)-Gamma.logGamma(deleted.getOrElse(SimpleLRContext(Consonant,Consonant),0)+realized.getOrElse(SimpleLRContext(Consonant,Consonant),0)+priorDrop+priorNodrop)+
			  	Gamma.logGamma(priorDrop+priorNodrop)-Gamma.logGamma(priorDrop)-Gamma.logGamma(priorNodrop)+	        
			  	Gamma.logGamma(deleted.getOrElse(SimpleLRContext(Consonant,Vowel),0)+priorDrop)+Gamma.logGamma(realized.getOrElse(SimpleLRContext(Consonant,Vowel),0)+priorNodrop)-Gamma.logGamma(deleted.getOrElse(SimpleLRContext(Consonant,Vowel),0)+realized.getOrElse(SimpleLRContext(Consonant,Vowel),0)+priorDrop+priorNodrop)+
			  	Gamma.logGamma(priorDrop+priorNodrop)-Gamma.logGamma(priorDrop)-Gamma.logGamma(priorNodrop)+
			  	Gamma.logGamma(deleted.getOrElse(SimpleLRContext(Consonant,Pause),0)+priorDrop)+Gamma.logGamma(realized.getOrElse(SimpleLRContext(Consonant,Pause),0)+priorNodrop)-Gamma.logGamma(deleted.getOrElse(SimpleLRContext(Consonant,Pause),0)+realized.getOrElse(SimpleLRContext(Consonant,Pause),0)+priorDrop+priorNodrop)+
			  	Gamma.logGamma(priorDrop+priorNodrop)-Gamma.logGamma(priorDrop)-Gamma.logGamma(priorNodrop)+
			  	Gamma.logGamma(deleted.getOrElse(SimpleLRContext(Vowel,Consonant),0)+priorDrop)+Gamma.logGamma(realized.getOrElse(SimpleLRContext(Vowel,Consonant),0)+priorNodrop)-Gamma.logGamma(deleted.getOrElse(SimpleLRContext(Vowel,Consonant),0)+realized.getOrElse(SimpleLRContext(Vowel,Consonant),0)+priorDrop+priorNodrop)+
			  	Gamma.logGamma(priorDrop+priorNodrop)-Gamma.logGamma(priorDrop)-Gamma.logGamma(priorNodrop)+	        
			  	Gamma.logGamma(deleted.getOrElse(SimpleLRContext(Vowel,Vowel),0)+priorDrop)+Gamma.logGamma(realized.getOrElse(SimpleLRContext(Vowel,Vowel),0)+priorNodrop)-Gamma.logGamma(deleted.getOrElse(SimpleLRContext(Vowel,Vowel),0)+realized.getOrElse(SimpleLRContext(Vowel,Vowel),0)+priorDrop+priorNodrop)+
			  	Gamma.logGamma(priorDrop+priorNodrop)-Gamma.logGamma(priorDrop)-Gamma.logGamma(priorNodrop)+
			  	Gamma.logGamma(deleted.getOrElse(SimpleLRContext(Vowel,Pause),0)+priorDrop)+Gamma.logGamma(realized.getOrElse(SimpleLRContext(Vowel,Pause),0)+priorNodrop)-Gamma.logGamma(deleted.getOrElse(SimpleLRContext(Vowel,Pause),0)+realized.getOrElse(SimpleLRContext(Vowel,Pause),0)+priorDrop+priorNodrop)+
			  	Gamma.logGamma(priorDrop+priorNodrop)-Gamma.logGamma(priorDrop)-Gamma.logGamma(priorNodrop)
		     case FIXDROP(_) =>
		        deleted.getOrElse(SimpleLRContext(Consonant,Consonant),0)*math.log(0.62)+realized.getOrElse(SimpleLRContext(Consonant,Consonant),0)*math.log(1-0.62)+     
			  	deleted.getOrElse(SimpleLRContext(Consonant,Vowel),0)*math.log(0.42)+realized.getOrElse(SimpleLRContext(Consonant,Vowel),0)*math.log(1-0.42)+
			  	deleted.getOrElse(SimpleLRContext(Consonant,Pause),0)*math.log(0.36)+realized.getOrElse(SimpleLRContext(Consonant,Pause),0)*math.log(1-0.36)+
			  	deleted.getOrElse(SimpleLRContext(Vowel,Consonant),0)*math.log(0.23)+realized.getOrElse(SimpleLRContext(Vowel,Consonant),0)*math.log(1-0.23)+
			  	deleted.getOrElse(SimpleLRContext(Vowel,Vowel),0)*math.log(0.15)+realized.getOrElse(SimpleLRContext(Vowel,Vowel),0)*math.log(1-0.15)+
			  	deleted.getOrElse(SimpleLRContext(Vowel,Pause),0)*math.log(0.07)+realized.getOrElse(SimpleLRContext(Vowel,Pause),0)*math.log(1-0.07)
			  
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
	  case DROPSYMBOL =>
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
    	case WBoundaryDrop | WBoundaryNodrop | UBoundaryDrop | UBoundaryNodrop => cPos
    	case NoBoundary =>  _findBoundary(op)(op(cPos))
	}
   

	def boundaryToLeft: (Int=>Int) = _findBoundary(_-1)
	def boundaryToRight: (Int=>Int) = _findBoundary(_+1)

	/**
	 * returns a triple (observed,underlying,withDrop)
	 */
	def getWordWithVar(sPos: Int, ePos: Int): (WordType,WordType,WordType) = {
	  val word = data.subList(sPos, ePos)
	  val wD = suffix(word,DROPSEG)
	  boundaries(ePos) match {
	    case UBoundaryDrop | WBoundaryDrop =>
	      (word,wD,wD)
	    case _ =>
	      (word,word,wD)
	  }
	}
	
	/**
	 * returns a tuple (observed,underlying)
	 * 
	 * this may save the additional cost of creating a copy of the token with an additional dropsegment
	 */
	def getWord(sPos: Int, ePos: Int): (WordType,WordType) = {
	  val word = data.subList(sPos, ePos)
	  boundaries(ePos) match {
	    case UBoundaryDrop | WBoundaryDrop =>
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
	      	case WBoundaryDrop => {
 	      	  res.append(wToS(suffix(data.subList(sPos-1, cPos),DROPSEG))+"::")
 	      	  inner(cPos+1,cPos+1,res)
	      	}
	      	case WBoundaryNodrop => {
 	      	  res.append(wToS(data.subList(sPos-1, cPos))+"::")
 	      	  inner(cPos+1,cPos+1,res)
	      	}
	      	case UBoundaryDrop => {
	      	  res.append(wToS(suffix(data.subList(sPos-1, cPos),DROPSEG))+"\n")
 	      	  inner(cPos+1,cPos+1,res)
	      	}
	      	case UBoundaryNodrop => {
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
	      	case WBoundaryDrop => {
 	      	  out.print(
 	      	      wToS(suffix(data.subList(sPos-1, cPos),DROPSEG))+sep)
 	      	  inner(cPos+1,cPos+1)
	      	}
	      	case WBoundaryNodrop => {
 	      	  out.print(wToS(data.subList(sPos-1, cPos))+sep)
 	      	  inner(cPos+1,cPos+1)
	      	}
	      	case UBoundaryDrop => {
	      	  out.print(wToS(suffix(data.subList(sPos-1, cPos),DROPSEG))+"\n")
 	      	  inner(cPos+1,cPos+1)
	      	}
	      	case UBoundaryNodrop => {
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
	      	case WBoundaryDrop => {
 	      	  out.print("t")
 	      	  inner(cPos+1,cPos+1)
	      	}
	      	case WBoundaryNodrop => {
 	      	  out.print("1")
 	      	  inner(cPos+1,cPos+1)
	      	}
	      	case UBoundaryDrop => {
	      	  out.print("t\n")
 	      	  inner(cPos+1,cPos+1)
	      	}
	      	case UBoundaryNodrop => {
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
		        case WBoundaryDrop =>
		          trueBoundaries+=1
		          trueTokens+=1
		          trueStartPos=i
		          trueDrops+=1
		        case WBoundaryNodrop =>
		          trueBoundaries+=1
		          trueTokens+=1
		          trueStartPos=i		          
		        case _ =>
		      }
		    }
		    case WBoundaryDrop => {
		      totalBoundaries+=1
		      totalTokens+=1
		      totalDrops+=1
		      goldBoundaries(i) match {
		        case WBoundaryDrop | WBoundaryNodrop => {
		          trueBoundaries+=1
		          correctBoundaries+=1
		          trueTokens+=1
		          // don't punish for wrong t-postulation
		          //if (predStartPos==trueStartPos && goldBoundaries(i)==boundaries(i)) correctTokens+=1
		          if (predStartPos==trueStartPos) correctTokens+=1		          
		          trueStartPos=i
		          if (goldBoundaries(i)==WBoundaryDrop) {
		            trueDrops+=1
		            correctDrops+=1
		          }
		        }
		        case _ =>
		      }
		      predStartPos=i
		    }
		    case WBoundaryNodrop => {
		      totalBoundaries+=1
		      totalTokens+=1
		      goldBoundaries(i) match {
		        case WBoundaryDrop | WBoundaryNodrop => {
		          trueBoundaries+=1
		          correctBoundaries+=1
		          trueTokens+=1
		          if (predStartPos==trueStartPos) correctTokens+=1
		          trueStartPos=i
		          if (goldBoundaries(i)==WBoundaryDrop) {
		            trueDrops+=1
		          }		          
		        }
		        case _ =>
		      }
		      predStartPos=i
		    }
		    case UBoundaryDrop | UBoundaryNodrop => {
		      totalTokens+=1
		      trueTokens+=1
		      if (boundaries(i)==UBoundaryDrop) totalDrops+=1
		      if (goldBoundaries(i)==UBoundaryDrop) {
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