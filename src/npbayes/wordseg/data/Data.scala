package npbayes.wordseg.data

/**
 * we define a simple beta-prior over the application of each variation rule. consequently, we
 * need to track the number of times with which we observed each individual rule applying / not applying
 */


import npbayes.wordseg
import npbayes.WordType
import java.io._
import scala.collection.mutable.HashMap
//import scala.collection.mutable.OpenHashMap
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
import npbayes.WordType
import npbayes.WordType



abstract class DeletionModel
case object GlobalFix extends DeletionModel
case object CVPFollows extends DeletionModel
case object CVPLeftRight extends DeletionModel

abstract class Boundary
case object NoBoundary extends Boundary
case object WBoundary extends Boundary
case object UBoundary extends Boundary

abstract class Rule
case object Del1 extends Rule
case object Rel1 extends Rule
case object Del2 extends Rule
case object Rel2 extends Rule
case object NoRule extends Rule

abstract class RuleContext
case class SimpleRContext(rC: PhonemeType) extends RuleContext
case class SimpleLRContext(lC: PhonemeType,rC: PhonemeType) extends RuleContext

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

class Data(fName: String, val dropProb: Double = 0.0,val MISSING1: String = "*", val DROP1: String = "T", val MISSING2: String="DROPD", val DROP2: String = "D", val delModeLType: DeletionModel = GlobalFix) {
	val _random = new Random()
	//Special Characters
	val UBOUNDARYSYMBOL="UTTERANCEBOUNDARY"
	val DROPSEG1=SymbolSegTable(DROP1)
	val DROPSEG2=SymbolSegTable(DROP2)
	def isConsonant = wordseg.wordseg.isConsonant
	def isVowel(x: SegmentType) = wordseg.wordseg.isVowel(x)
	def isPause = wordseg.wordseg.isPause	
	//Rules
	var nD1 = 0
	var nNotD1 = 0
	var nD2 = 0
	var nNotD2 = 0
	val deleted1: HashMap[RuleContext,Int] = new HashMap
	val realized1: HashMap[RuleContext,Int] = new HashMap
	val deleted2: HashMap[RuleContext,Int] = new HashMap
	val realized2: HashMap[RuleContext,Int] = new HashMap
	
	val sentences: ArrayBuffer[(Int,Int)] = new ArrayBuffer //(x,y) refers to a sentence spanning the characters from
	
	
	/**
	 * Initialize the data and goldBoundaries
	 */
	val (data,goldBoundaries,goldRules) = init
	val UBOUNDARYWORD=new WordType(Array.fill(1)(SymbolSegTable(UBOUNDARYSYMBOL)),0,1,-1) //always prefix corpus with an additional boundary symbol
	
	var boundaries = randomBoundaries(wordseg.wordseg.binitProb).toArray
	var rules = Array.fill[Rule](boundaries.length)(NoRule)

	def init() = {
		var seqPhones = Vector.empty[Int]
		var seqBoundaries: Vector[Boundary] = Vector.empty:+UBoundary
		var seqRules: Vector[Rule] = Vector.empty:+NoRule
		var startPos = 0
		var stringPos = 0
		def processLine(line: String) = {
			for (w <- line.stripLineEnd.split("\t")) {
				for (c: String <- w.split(" ")) {
				seqPhones = seqPhones:+ SymbolSegTable(c)
					seqBoundaries = seqBoundaries:+NoBoundary
					seqRules = seqRules:+NoRule
					stringPos+=1
				}
				//last symbole is Boundary, check for deleted symbols
				SymbolSegTable(seqPhones.last) match {
				  case MISSING1 =>
				    seqPhones = seqPhones.dropRight(1)
					seqBoundaries = seqBoundaries.dropRight(2):+WBoundary
					seqRules = seqRules.dropRight(2):+Del1
				  case DROP1 =>
				    seqBoundaries = seqBoundaries.dropRight(1):+WBoundary
				    seqRules = seqRules.dropRight(1):+ Rel1
				  case MISSING2 =>
				    seqPhones = seqPhones.dropRight(1)
					seqBoundaries = seqBoundaries.dropRight(2):+WBoundary
					seqRules = seqRules.dropRight(2):+Del2
				  case DROP2 =>
				    seqBoundaries = seqBoundaries.dropRight(1):+WBoundary
				    seqRules = seqRules.dropRight(1):+ Rel2				    
				  case _ =>
				    seqBoundaries = seqBoundaries.dropRight(1):+WBoundary
				    seqRules = seqRules.dropRight(1):+ NoRule				    
				}
			}
	       // adjust for word-boundaries --- last NoBoundary is in fact a word-boundary			
			seqBoundaries = seqBoundaries.last match {
				case WBoundary => seqBoundaries.dropRight(1):+UBoundary
			}
			sentences+=((startPos,stringPos))
			startPos=stringPos
		}
		for (l <- Source.fromInputStream(new FileInputStream(fName), "utf-8").getLines) processLine(l)
		(seqPhones.toArray,seqBoundaries.toArray,seqRules.toArray)
	}

	def segmentToType(segment: SegmentType): PhonemeType = PhonemeClassMap(segment)
	  
	def addContext(hm: HashMap[RuleContext,SegmentType],lSegment: SegmentType,rSegment: SegmentType) = {
	  wordseg.wordseg.dropInferenceMode match {
	    case INFERDROP(_,_) =>
	    	delModeLType match {
	    		case CVPFollows =>
	    			hm.put(SimpleRContext(segmentToType(rSegment)),
	    					hm.getOrElse(SimpleRContext(segmentToType(rSegment)), 0)+1)
	    		case CVPLeftRight =>
	    			hm.put(SimpleLRContext(segmentToType(lSegment),segmentToType(rSegment)),
	    					hm.getOrElse(SimpleLRContext(segmentToType(lSegment),segmentToType(rSegment)),0)+1)
	    		case _ =>
	      
	    	}
	    case _ =>
	  }
	  
	}
	
	def addDrop1(lSegment: SegmentType, rSegment: SegmentType) = {
	  nD1+=1
	  addContext(deleted1,lSegment,rSegment)
	}

	def addDrop2(lSegment: SegmentType, rSegment: SegmentType) = {
	  nD2+=1
	  addContext(deleted1,lSegment,rSegment)
	}
	
	def addNoDrop1(lSegment: SegmentType, rSegment: SegmentType) = {
	  nNotD1 += 1
	  addContext(realized1,lSegment,rSegment)
	}

	def addNoDrop2(lSegment: SegmentType, rSegment: SegmentType) = {
	  nNotD2 += 1
	  addContext(realized2,lSegment,rSegment)
	}	

	def removeContext(hm: HashMap[RuleContext,Int],lSegment: SegmentType, rSegment: SegmentType) = {
	  wordseg.wordseg.dropInferenceMode match {
	    case INFERDROP(_,_) =>
		  delModeLType match {
		    case CVPFollows =>
		      val old = hm.get(SimpleRContext(segmentToType(rSegment)))
			  old match {
			    case Some(x) =>
			      if (x-1==0)
			        hm.remove(SimpleRContext(segmentToType(rSegment)))
			      else
			        hm.put(SimpleRContext(segmentToType(rSegment)),x-1)
			    case None =>
			      throw new Error("In removeDrop("+rSegment+")")
			  }
		    case CVPLeftRight =>
		      val old = hm.get(SimpleLRContext(segmentToType(lSegment),segmentToType(rSegment)))
			  old match {
			    case Some(x) =>
			      if (x-1==0)
			        hm.remove(SimpleLRContext(segmentToType(lSegment),segmentToType(rSegment)))
			      else
			        hm.put(SimpleLRContext(segmentToType(lSegment),segmentToType(rSegment)),x-1)
			    case None =>
			      throw new Error("In removeDrop("+lSegment+","+rSegment+")")
			  	      
		      }
		    case _ =>	      
		  }
	    case _ =>
	  }     
	}	
	
	def removeDrop1(lSegment: SegmentType, rSegment: SegmentType) = {
	  nD1 -= 1
	  removeContext(deleted1,lSegment,rSegment)
	}
	
	def removeDrop2(lSegment: SegmentType, rSegment: SegmentType) = {
	  nD2 -= 1
	  removeContext(deleted2,lSegment,rSegment)
	}
	
	def removeNoDrop1(lSegment: SegmentType, rSegment: SegmentType) = {
	  nNotD1 -= 1
	  removeContext(realized1,lSegment,rSegment)
	}
	
	def removeNoDrop2(lSegment: SegmentType, rSegment: SegmentType) = {
	  nNotD2 -= 1
	  removeContext(realized2,lSegment,rSegment)
	}	

	def whichTransform(observed: WordType, underlying: WordType): Rule = {
	  val uEndsIn = underlying.lastSeg
	  val sameLength = observed.size==underlying.size
	  if (underlying.size>1) {
	    uEndsIn match {
	      case DROPSEG1 => if (sameLength) Rel1 else Del1
	      case DROPSEG2 => if (sameLength) Rel2 else Del2 
	      case _ => NoRule
	    }
	  } else {
	    NoRule
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
	def removeTransformation(rule: Rule, underlying: WordType,rightContext: WordType) = {
	  rule match {
	    case Rel1 =>
          removeNoDrop1(underlying(underlying.length-2), rightContext(0))
	    case Del1 =>
	      removeDrop1(underlying(underlying.length-2),rightContext(0))
	    case Rel2 =>
	      removeNoDrop2(underlying(underlying.length-2), rightContext(0))
	    case Del2 =>
	      removeDrop2(underlying(underlying.length-2), rightContext(0))	      
	    case NoRule =>
	  }
	}

	def addTransformation(rule: Rule, underlying: WordType,rightContext: WordType) = {
	  rule match {
	    case Rel1 =>
	        addNoDrop1(underlying(underlying.length-2), rightContext(0))
	    case Del1 =>
	        addDrop1(underlying(underlying.length-2),rightContext(0))
	    case Rel2 =>
	        addNoDrop2(underlying(underlying.length-2), rightContext(0))
	    case Del2 =>
	        addDrop2(underlying(underlying.length-2),rightContext(0))
	    case NoRule =>
	  }
	}
	
	
	/**
	 * @return	length of the sentence (= number of boundaries)
	 */
	def getSentenceLength(x:(Int, Int)): Int =
	  x._2-x._1

	/**
	 * randomize boundaries
	 */
	def randomBoundaries(boundProb: Double=0.0) = {
		var seqBoundaries: Vector[Boundary] = Vector.empty
		for (b <- goldBoundaries) {
			b match {
				case UBoundary => seqBoundaries=seqBoundaries:+UBoundary
				case WBoundary | NoBoundary => seqBoundaries=seqBoundaries:+{
					if (_random.nextDouble>boundProb)
						NoBoundary 
					else 
						WBoundary
				}
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
	  case CVPLeftRight => dropProbContext(s(s.length-2),rContext)	  	    
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
		        throw new Error("Unknown Segment "+SymbolSegTable(rContext))	      
	  }

	}//dropProb	//TODO - word-specific probabilities  */
	
	def setBoundary(pos: Int, b: Boundary): Unit = 
	  boundaries(pos)= b

	def setRule(pos: Int, b: Rule): Unit = 
	  rules(pos)= b	  
	  
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
	  val res = u.lastSeg match {
	  case DROPSEG1 =>
	    val rContext = rWord(rWord.length-1)
	    if (u==s)
	      (1-dropProb(u,rContext))
	    else
	      if (u.allButLast==s)
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
    	case WBoundary | UBoundary => cPos
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
	  val word = new WordType(data,sPos, ePos,-1)
	  val wD = new WordType(data,sPos,ePos,DROPSEG1)
	  rules(ePos) match {
	    case Del1 =>
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
	  val word = new WordType(data,sPos, ePos,-1)
	  rules(ePos) match {
	    case Del1 =>
	      (word,new WordType(data,sPos,ePos,DROPSEG1))
	    case _ =>
	      (word,word)
	  }
	}


	def getAnalysis(segSep: String, wordSeg: String): String = {
	  def inner(sPos: Int,cPos: Int,res: StringBuilder): String = 
	    if (cPos>=boundaries.size)
	      res.toString
	    else 
	      boundaries(cPos) match {
	      	case NoBoundary => inner(sPos,cPos+1,res)
	      	case WBoundary =>
	      	  rules(cPos) match {
	      	    case NoRule | Rel1 | Rel2 =>
	      	      res.append(wToS(new WordType(data,sPos-1,cPos,-1),segSep)+wordSeg)
	      	      inner(cPos+1,cPos+1,res)
	      	    case Del1 =>
	      	      res.append(wToS(new WordType(data,sPos-1,cPos,DROPSEG1),segSep)+wordSeg)
	      	      inner(cPos+1,cPos+1,res)
	      	    case Del2 =>
	      	      res.append(wToS(new WordType(data,sPos-1, cPos,DROPSEG2),segSep)+"\n")
	      	      inner(cPos+1,cPos+1,res)	      	      
	      	  }
	      	case UBoundary =>
	      	  rules(cPos) match {
	      	    case NoRule | Rel1 | Rel2 =>
	      	      res.append(wToS(new WordType(data,sPos-1, cPos,-1),segSep)+"\n")
	      	      inner(cPos+1,cPos+1,res)
	      	    case Del1 =>
	      	      res.append(wToS(new WordType(data,sPos-1, cPos,DROPSEG1),segSep)+"\n")
	      	      inner(cPos+1,cPos+1,res)	      	      
	      	    case Del2 =>
	      	      res.append(wToS(new WordType(data,sPos-1, cPos,DROPSEG2),segSep)+"\n")
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
	      	case NoBoundary => 
	      	  out.print("0")
	      	case WBoundary => 
	      	  rules(cPos) match {
	      	    case Del1 =>
	      	      out.print(DROP1)
	      	    case Del2 =>
	      	      out.print(DROP2)
	      	    case NoRule | Rel1 | Rel2 =>
	      	      out.print("1")
	      	  }
	    
	      	case UBoundary => 
	      	  rules(cPos) match {
	      	    case Del1 =>
	      	      out.print(DROP1)
	      	    case Del2 =>
	      	      out.print(DROP2)
	      	    case NoRule | Rel1 | Rel2 =>
	      	      out.print("1")
	      	  }
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
		for (i <- 1 to boundaries.length-1) {
		  rules(i) match {
		    case Del1 =>
		      totalDrops+=1
		    case _ =>
		  }
		  goldRules(i) match {
		    case Del1 =>
		      trueDrops+=1
		    case _ =>
		  }
		  correctDrops += { if (goldRules(i)==rules(i) && rules(i)==Del1) 1 else 0}
		  boundaries(i) match {
		    case NoBoundary => {
		      goldBoundaries(i) match {
		        case WBoundary =>
		          trueBoundaries+=1
		          trueTokens+=1
		          trueStartPos=i
		          trueDrops+=1          
		        case _ =>
		      }
		    }
		    case WBoundary => {
		      totalBoundaries+=1
		      totalTokens+=1
		      goldBoundaries(i) match {
		        case WBoundary => {
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
		    case UBoundary => {
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
	
	def showDropProbs: String = delModeLType match {
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
	
}