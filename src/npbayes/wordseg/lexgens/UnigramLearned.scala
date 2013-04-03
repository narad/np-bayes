package npbayes.wordseg.lexgens

import npbayes.distributions.PosteriorPredictive
import npbayes.wordseg.data.WordType
import npbayes.wordseg.data.SegmentType
import scala.collection.mutable.HashMap
import org.apache.commons.math3.special.Gamma
import java.util.{Iterator => JIterator}


/**
 * we assume a uniform dirichlet prior, with minor simplifications in the implementation
 * in particular, we don't renormalize the distribution as to adjust for "empty" words
 * and the fact that the WB can only occur at the end of a word
 * also, we won't consider word-internal changes to predictive probability
 */

class UnigramLearned(val nSegments: Int, val pseudoCount: Double = 0.01, val vowelConstraint: Boolean = false) extends PosteriorPredictive[WordType] {
  val WB: SegmentType = -1000
  val normalizer: Double = nSegments*pseudoCount
  var obsCounts: Int = 0 //total number of observed segments
  val phonCounts: HashMap[SegmentType,Int] = new HashMap //individual counts for observations
  def isVowel = npbayes.wordseg.wordseg.isVowel

  
  def _predPhon(seg: SegmentType) = {
    val nC = phonCounts.getOrElse(seg, 0)
    (nC+pseudoCount)/(obsCounts+normalizer)
  }
  
  /**
   * slight simplification --- no intermediate update
   */
  def predProb(obs: WordType): Double = {
	    var hasVowel: Boolean = false
    	var p = 1.0
	    val segs = obs.iterator()
	    while (segs.hasNext()) {
	      val seg = segs.next()
	      if (isVowel(seg))
	        hasVowel = true
	      p = p*_predPhon(seg)
	      // _addPhon(seg) //uncomment for 'exactness'
	    }
	    val res = p*_predPhon(WB)
	    /** uncomment for 'exactness'
		val segs2 = obs.iterator()
	    while (segs2.hasNext()) {
	      _removePhon(seg2.next()) 
	    }
	     */
	  if (hasVowel || !vowelConstraint)
		res
	  else
	    0.0
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
    	var hasVowel = false
	    var p=1.0
	    val segs = obs.iterator()
	    while (segs.hasNext()) {
	      val seg = segs.next()
	      if (isVowel(seg))
	        hasVowel = true
	      p = p*_predPhon(seg)
	      _addPhon(seg)
	    }
	    val res = p*_predPhon(WB)
	    _addPhon(WB)
	    if (hasVowel || !vowelConstraint)
          res
	    else
	      throw new Error("Can't add word without vowel: "+ npbayes.wordseg.data.wToS(obs))
  }

  def remove(obs: WordType): Double = { 
	    var p=1.0
	    val segs = obs.reverse().iterator()
	    _removePhon(WB)
	    p=p*_predPhon(WB)
	    while (segs.hasNext()) {
	      val seg = segs.next()
	      _removePhon(seg)
	      p*=_predPhon(seg)
	    }
	    p
  }
  
  
  /**
   * standard DirichletMultinomialCompound,
   */
  override def logProb = {
    Gamma.logGamma(normalizer)-Gamma.logGamma(obsCounts+normalizer)+
    	{ for (count: Int <- phonCounts.values.toList)
    		yield (Gamma.logGamma(count+pseudoCount)-Gamma.logGamma(pseudoCount)) }.sum 
  }
}