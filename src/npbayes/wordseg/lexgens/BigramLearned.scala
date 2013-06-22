package npbayes.wordseg.lexgens

import npbayes.distributions.PosteriorPredictive
import npbayes.WordType
import npbayes.wordseg.data.SegmentType
import java.util.HashMap
import org.apache.commons.math3.special.Gamma
import java.util.{Iterator => JIterator}
import npbayes.wordseg.data.SymbolSegTable
import npbayes.wordseg.data.SymbolClassTable
import npbayes.wordseg.data.PhonemeClassMap


/**
 * we assume a uniform dirichlet prior, with minor simplifications in the implementation
 * in particular, we don't consider word-internal changes to predictive probability
 * 
 * this corresponds to a grammar of the form
 * Word --> Segs     [1.0]
 * Segs --> Seg      [a]
 * Segs --> Segs Seg [1-a]
 * 
 * with _predWB giving the probability a of using the terminating rule which is learned
 * assuming a symmetric 1,1-Beta prior.
 * 
 * the vowel-constraint just takes away mass, but the renormalization is done trivially if required
 */

class BigramLearned(val nSegments: Int, val UB: WordType, val pUB: Double=0.5, val pseudoCount: Double = 0.01, val vowelConstraint: Boolean = false) extends PosteriorPredictive[WordType] {
  def isVowel(x: SegmentType) =SymbolClassTable(PhonemeClassMap.getClass(x))=="VOWEL"
  val normalizer: Double = nSegments*pseudoCount
  var obsCounts: Int = 0 //total number of observed segments
  var wordCount: Int = 0
  var branchCount: Int = 0
  var utCount: Int = 0 //number of generated utterance-boundary symbols  
  val phonCounts: Array[SegmentType] = Array.fill(SymbolSegTable.nSymbols)(0)
  
  def _predWB =
    (wordCount + 1) / (wordCount+branchCount+2.0)
  
  def _predPhon(seg: SegmentType) = {
    val nC: Int = phonCounts(seg-1) 
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
	    val continue = (1-_predWB)
	    for (seg <- obs) {
	      if (!vowelConstraint || isVowel(seg))
	        hasVowel = true
	      p = p*_predPhon(seg)
	      if (!vowelConstraint || hasVowel)
	        p = p*continue
	    }
	    val res = (1-pUB)*p*((1-continue)/continue)
	    if (hasVowel || !vowelConstraint)
	     res
	    else
	     0
    }
  }
  
  def _addPhon(s: SegmentType) = {
	phonCounts(s-1)+=1
    obsCounts+=1
  }
  
  def _removePhon(s: SegmentType) = {
	phonCounts(s-1)-=1
	if (phonCounts(s-1)<0)
      throw new Error("can't remove "+s+" in BigramLearned._removePhone")
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
	    var i = 0
	    while (i<obs.size-1) {
	      if (!vowelConstraint || isVowel(obs(i)))
	        hasVowel = true
	      p = p*_predPhon(obs(i))
	      if (hasVowel || !vowelConstraint) {
	        p = p * (1-_predWB)
	        branchCount+=1
	      }
	      _addPhon(obs(i))
	      i+=1
	    }
	    if (!vowelConstraint || isVowel(obs(i)))
	      hasVowel=true
	    val res = (1-pUB)*p*_predPhon(obs(i))*_predWB
	    _addPhon(obs(i))
	    wordCount+=1
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
        // find the j such that there are no until position j
        var j = 0
        var foundV = false
        while (j<obs.size && !foundV) {
          if (vowelConstraint && isVowel(obs(j)))
            foundV=true
          else
            j+=1
        }
	    var p=1.0
	    var i=obs.size-1
	    wordCount-=1
	    _removePhon(obs(i))
	    p=p*_predWB*_predPhon(obs(i))
	    i-=1
	    while (i>=0) {
	      _removePhon(obs(i))
	      p*=_predPhon(obs(i))
	      if (i>=j || !vowelConstraint) {
	        branchCount-=1
	        p*=(1-_predWB)
	      }
	      i-=1
	    }
	    (1-pUB)*p
    }
  }
  
  
  /**
   * first part is your standard DirichletMultinomialCompound,
   * second part is the little Bigram-model trickery for Utterance Boundaries
   */
  override def logProb = {
    var result: Double = Gamma.logGamma(normalizer)-Gamma.logGamma(obsCounts+normalizer)
    var i = 0
    while (i<phonCounts.length) {
      result +=Gamma.logGamma(phonCounts(i)+pseudoCount)-Gamma.logGamma(pseudoCount)
      i+=1
    }
    result + math.log(pUB)*utCount + math.log(1-pUB)*wordCount + Gamma.logGamma(2.0)-Gamma.logGamma(wordCount+branchCount+2.0)+
    Gamma.logGamma(branchCount+1.0)-2*Gamma.logGamma(1.0)+Gamma.logGamma(wordCount+1.0)
 }
}