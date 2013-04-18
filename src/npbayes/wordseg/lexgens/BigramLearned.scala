package npbayes.wordseg.lexgens

import npbayes.distributions.PosteriorPredictive
import npbayes.WordType
import npbayes.wordseg.data.SegmentType
import npbayes.wordseg.HM
import scala.collection.mutable.HashMap
import org.apache.commons.math3.special.Gamma
import java.util.{Iterator => JIterator}


/**
 * we assume a uniform dirichlet prior, with minor simplifications in the implementation
 * in particular, we don't renormalize the distribution as to adjust for "empty" words
 * and the fact that the WB can only occur at the end of a word
 * also, we won't consider word-internal changes to predictive probability
 * 
 * the vowel-constraint just takes away mass, but the renormalization is done trivially if required
 */

class BigramLearned(val nSegments: Int, val UB: WordType, val pUB: Double=0.5, val pseudoCount: Double = 0.01, val vowelConstraint: Boolean = false) extends PosteriorPredictive[WordType] {
  def isVowel = npbayes.wordseg.wordseg.isVowel
  val WB: SegmentType = -1000
  val normalizer: Double = nSegments*pseudoCount
  var obsCounts: Int = 0 //total number of observed segments
  var utCount: Int = 0 //number of generated utterance-boundary symbols  
  val phonCounts: HM[SegmentType,Int] = new HM //individual counts for observations
  
  def _predPhon(seg: SegmentType) = {
    val nC = phonCounts.getOrElse(seg, 0)
    (nC+pseudoCount)/(obsCounts+normalizer)
  }
  
  /**
   * simplification --- no intermediate update, no correction for vowel-mass
   */
  def predProb(obs: WordType): Double = {
    if (obs==UB)
      pUB
    else {
        var hasVowel: Boolean = false
	    var p = 1.0
	    for (seg <- obs) {
	      if (isVowel(seg))
	        hasVowel = true
	      p = p*_predPhon(seg)
	      // _addPhon(seg) //uncomment for 'exactness'
	    }
	    val res = (1-pUB)*p*_predPhon(WB)
	    /** uncomment for 'exactness'
		val segs2 = obs.iterator()
	    while (segs2.hasNext()) {
	      _removePhon(seg2.next()) 
	    }
	     */
	   if (hasVowel || !vowelConstraint)
	     res
	   else
	     0
    }
  }
  
  def _addPhon(s: SegmentType) = {
    val old = phonCounts.getOrElse(s, 0)
    phonCounts.put(s,old+1)
    obsCounts+=1
  }
  
  def _removePhon(s: SegmentType) = {
    val old = phonCounts.get(s) match {
      case Some(x) => x
      case _ => throw new Error("can't remove "+s+" in BigramLearned._removePhone")
    }
    if (old-1==0)
      phonCounts.remove(s)
    else
      phonCounts.put(s,old-1)
    obsCounts-=1
  }

  /**
   * returns exact probability
   */
  def update(obs:  WordType): Double = {
    if (obs==UB) {
      utCount+=1
      pUB 
    }
    else {
	    var p=1.0
	    var hasVowel = false
	    for (seg<-obs) {
	      if (isVowel(seg))
	        hasVowel = true
	      p = p*_predPhon(seg)
	      _addPhon(seg)
	    }
	    val res = (1-pUB)*p*_predPhon(WB)
	    _addPhon(WB)
	    if (hasVowel || !vowelConstraint)
	    	res
	    else
	      throw new Error("Can't add word without vowel: "+ npbayes.wordseg.data.wToS(obs))
    }
  }

  def remove(obs: WordType): Double = {
    if (obs==UB) {
      utCount-=1
      pUB
    }
    else { 
	    var p=1.0
	    _removePhon(WB)
	    p=p*_predPhon(WB)
	    for (seg <- obs.reverse) {
	      _removePhon(seg)
	      p*=_predPhon(seg)
	    }
	    (1-pUB)*p
    }
  }
  
  
  /**
   * first part is your standard DirichletMultinomialCompound,
   * second part is the little Bigram-model trickery for Utterance Boundaries
   */
  override def logProb = {
    Gamma.logGamma(normalizer)-Gamma.logGamma(obsCounts+normalizer)+
    	{ for (count: Int <- phonCounts.values.toList)
    		yield (Gamma.logGamma(count+pseudoCount)-Gamma.logGamma(pseudoCount)) }.sum +  
    math.log(pUB)*utCount +
    math.log(1-pUB)*phonCounts.getOrElse(WB, 0)
  }
}