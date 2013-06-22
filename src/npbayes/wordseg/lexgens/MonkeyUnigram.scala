package npbayes.wordseg.lexgens

import npbayes.distributions.PosteriorPredictive
import npbayes.WordType

/**
 * Monkey-Generator for the Unigram model
 * @author bborschi
 *
 * 
 */
class MonkeyUnigram(val nPhones: Int, val pStop: Double) extends PosteriorPredictive[WordType] {
  val _pPhon: Double = 1.0/nPhones
  var _nWords = 0
  var _nPhons = 0
  
  override def logProb =   {
    _nWords*math.log(pStop*_pPhon)+ //the first phone and the word-end
    (_nPhons-_nWords)*math.log((1-pStop)*_pPhon) //the word-internal phones
  }
  
  def remove(obs: WordType) = {
	val res = predProb(obs)
    _nWords-=1
	_nPhons-=obs.size
	res
  }
  def update(obs: WordType) = {
    val res = predProb(obs)
    _nWords+=1
    _nPhons+=obs.size
    res
  }
  def predProb(obs: WordType) =  _pPhon*math.pow(_pPhon*(1-pStop),obs.size-1)*pStop
}
