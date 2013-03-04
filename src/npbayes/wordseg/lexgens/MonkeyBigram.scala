package npbayes.wordseg.lexgens

import npbayes.distributions.PosteriorPredictive
import npbayes.wordseg.data.WordType

/**
 * Monkey-Generator for the Bigram model
 * 
 * NOTE: the dpseg2 implementation doesn't use this base-distribution, hence the results are not
 * comparable, unfortunately
 * this is, however, the distributions described in GGJ 2009 (and probably used in dpseg1.2 which
 * I can't verify as it won't compile)
 * 
 * @author bborschi
 *
 */
class MonkeyBigram(val nPhones: Int, val pStop: Double,val UB: WordType, val pUB: Double=0.5) extends PosteriorPredictive[WordType] {
  val _pPhon: Double = 1.0/nPhones
  var _nWords = 0
  var _nPhons = 0
  var _nUBS = 0
  var _logProb = 0.0
  
  /**
   * dpseg2-version: _nWords*math.log(pStop*_pPhon)+(_nPhons-_nWords)*math.log((1-pStop)*_pPhon)+
      _nUBS*math.log(pUB)
   */
  override def logProb = //_logProb  {
  {   
      _nWords*math.log((1-pUB)*pStop*_pPhon)+ //the first phone of each word, and its end
      (_nPhons-_nWords)*math.log((1-pStop)*_pPhon)+ //all the other phones of the individual words, and the continuation
      _nUBS*math.log(pUB) //the utterance boundaries
  }
  
  def remove(obs: WordType) = {
	val res = predProb(obs)
	_logProb -= math.log(res)
    if (obs==UB)
	  _nUBS-=1
	else {
	  _nWords-=1
	  _nPhons-=obs.size	    
	}
	res
  }
  def update(obs: WordType) = {
    val res = predProb(obs)
    _logProb += math.log(res)
    if (obs==UB)
      _nUBS+=1
    else {
      _nWords+=1
      _nPhons+=obs.size
    }
    res
  }
  
  /**
   * dpseg2 leaves out the (1-pUB) factor (which, strictly speaking, leads to deficient distributions,
   * and it actually seems to have some impact)
   */
  def predProb(obs: WordType) = 
      if (obs==UB) 
        pUB
      else
        (1-pUB)* //generate a word, not a boundary
        _pPhon*math.pow(_pPhon*(1-pStop),obs.size-1)*pStop //generate the word
}
