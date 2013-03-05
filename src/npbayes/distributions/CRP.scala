package npbayes.distributions
import npbayes.Utils
import npbayes.wordseg

import scala.collection.mutable.HashMap
import scala.util.Random
import scala.collection.mutable.WeakHashMap


import scala.collection.mutable.LinkedList

import org.apache.commons.math3.special.Gamma
import java.util.TreeMap
import java.util.Map.{Entry => JEntry}
import java.util.{Iterator => JIterator}

import npbayes.wordseg._

abstract class HEURISTIC
case object EXACT extends HEURISTIC
case object MINPATH extends HEURISTIC
case object MAXPATH extends HEURISTIC

/**
 * this follows closely Sharons's/Mark's implementation
 * I found this way of bookkeeping more efficient than maintaining
 * several independent Maps (obs->counts, obs->tableCounts, obs->tables)
 * 
 * not sure whether the java-TreeMap is really necessary and whether the
 * internal conversions lead to huge performance losses
 * @author bborschi
 *
 */
class TypeCount {
  var nCust = 0
  var nTables = 0
  
  val nCust_nTables: TreeMap[Int,Int] = new TreeMap
  
  def isEmpty: Boolean =
    nCust==0 && nTables==0
  
  def sanity: Boolean = {
    var nn = 0
    var mm = 0
    val entries : JIterator[JEntry[Int, Int]] = nCust_nTables.entrySet().iterator
    while (entries.hasNext()) {
    	val entry: JEntry[Int,Int] = entries.next();
    	nn += entry.getKey()*entry.getValue()
    	mm += entry.getValue()
    }
    assert(nn==nCust)
    assert(mm==nTables)
    nn==nCust && mm==nTables
  }
    
  def sitAtNew = {
    nCust+=1
    nTables+=1
    Option(nCust_nTables.get(1)) match {
      case Some(x) =>
        nCust_nTables.put(1,x+1)
      case None =>
        nCust_nTables.put(1, 1)
    }
  }
  
  def sitAtOld(r: Double, discount: Double): Int = {
	def inner(tables: JIterator[JEntry[Int,Int]],current: Double): Int = 
     if (!tables.hasNext())
        throw new Error("Couldn't add to table")
      else {
        val entry = tables.next()
        val tableSize = entry.getKey()
        val nTables = entry.getValue()
        if (current-(tableSize-discount)*nTables<=0) {        
          val n1 = tableSize+1 //one more table with that many customers
          if (nTables-1==0) //no more tables of this size
            nCust_nTables.remove(tableSize)
          else
            nCust_nTables.put(tableSize,nTables-1)
          val old = Option(nCust_nTables.get(n1))
          old match {
            case Some(x) =>
              nCust_nTables.put(n1, x+1)
            case None =>
              nCust_nTables.put(n1, 1)
          }
          tableSize //we sit at this table
        }
        else
          inner(tables,current-(tableSize-discount)*nTables)
      }
	val res = inner(nCust_nTables.entrySet.iterator,r)
    nCust+=1
    res
  }
  
  def remove(r: Int): Int = {
	def inner(tables: JIterator[JEntry[Int,Int]],current: Int=0): Int = 
      if (!tables.hasNext)
        throw new Error("Couldn't remove")
      else {
        val entry = tables.next
        val tableSize = entry.getKey()
        val nTs = entry.getValue()
        if (r<=current+tableSize*nTs) {
          val n1 = tableSize-1 //one more table of this size
          if (nTs-1 == 0)
            nCust_nTables.remove(tableSize)
          else
            nCust_nTables.put(tableSize,nCust_nTables.get(tableSize)-1)
          if (n1==0) //one less table
            nTables-=1
          else {
            Option(nCust_nTables.get(n1)) match {
              case Some(x) =>
                nCust_nTables.put(n1, x+1)
              case None =>
                nCust_nTables.put(n1, 1)
            }
          }
          nCust-=1
          n1
        } else
          inner(tables, current+tableSize*nTs)
      }
	inner(nCust_nTables.entrySet.iterator,0)
  }
}

class CRP[T](var concentration: Double, var discount: Double, val base: PosteriorPredictive[T], val assumption: HEURISTIC=EXACT) extends PosteriorPredictive[T] {
  val _random = new Random()
  
  val labelTabels: HashMap[T,TypeCount] = new HashMap
  val emptyCount = new TypeCount
  
  def _oCount(o: T): Int = labelTabels.getOrElse(o, emptyCount).nCust//hmObsCounts.getOrElse(o, 0)
  var _oCount = 0
  def _tCount(o: T): Int = labelTabels.getOrElse(o, emptyCount).nTables//hmTableCounts.getOrElse(o, 0)
  
  
  var _tCount = 0
  
  /**
   * predictive probability, taking into account the additional
   * observation prev
   */
  def apply(obs: T,prevs: List[T]) = {
    for (prev <- prevs)
      update(prev)
    val res=predProb(obs)
    for (prev <- prevs)
      remove(prev)
    res
  }
  
    def sanityCheck: Boolean = {
      var nn=0
      var tt=0
      for ((word,counts) <- labelTabels.toList) {
        assert(counts.sanity)
        nn+=counts.nCust
        tt+=counts.nTables
      }
      assert(nn==_oCount)
      assert(tt==_tCount)
      nn==_oCount && tt==_tCount
  	}
  
  def isEmpty: Boolean = _oCount==0
  
  /**
   * full logProb, including base-distribution
   */
  override def logProb = {
    logProbSeating + base.logProb
  }
  
  def logProbSeating =
    _logProbSeating(concentration,discount)

  def logProbSeating(c: Double) =
    _logProbSeating(c,discount)
    
  /**
   * just the seating-arrangement
   */
  def _logProbSeating(concentration: Double, discount: Double): Double = {
    //cf e.g. Goldwate et al., 2011, p.2342 (1-Param,discount=0) and p.2345 (2-Param)
    var res = Gamma.logGamma(concentration)-Gamma.logGamma(_oCount+concentration)
    for (tokenCount <- labelTabels.values) {
      val iter : JIterator[JEntry[Int,Int]] = tokenCount.nCust_nTables.entrySet().iterator()
      while (iter.hasNext()) {
        val entry = iter.next()
        val nC = entry.getKey()
        val nT = entry.getValue()
        res += ((Gamma.logGamma(nC-discount)-Gamma.logGamma(1-discount)))*nT
      }
/*      for ((nC,nT) <- tokenCount.nCust_nTables)
    	  res += ((Gamma.logGamma(nC-discount)-Gamma.logGamma(1-discount)))*nT*/
    }
    if (discount==0)
      res += _tCount*math.log(concentration)
    else
      res += (_tCount*math.log(discount)+Gamma.logGamma(concentration/discount+_tCount)-
    		  Gamma.logGamma(concentration/discount))
    res
  }
  
  def _pSitAtOld(obs: T) =
    if (_oCount==0)
      0
    else
      (_oCount(obs)-discount*_tCount(obs)) / (_oCount+concentration)
      
  def _pSitAtNew(obs: T) =
    (concentration+discount*_tCount)*base(obs) / (_oCount+concentration)

  
  def update (obs: T): Double = {
    assumption match  {
     case EXACT =>   
      val oldT = _oCount(obs)-discount*_tCount(obs)
      val newT = (concentration+discount*_tCount)*base(obs)
      val p = _random.nextDouble*(oldT+newT)
      _oCount+=1
	  if (p < oldT) {
	    val nCust = labelTabels(obs).sitAtOld(p,discount)
	    assert(nCust>0)
//	    oldT/(_oCount-1+concentration)
	    nCust/(_oCount-1+concentration)
	  } else {
	    labelTabels.getOrElseUpdate(obs, new TypeCount).sitAtNew
	    val mProb = base.update(obs)
	    _tCount+=1
	    //assert(sanityCheck)
	    concentration*mProb/(_oCount-1+concentration)
	  }
     case MINPATH => 
       if (_oCount(obs)==0) { 
         labelTabels.getOrElseUpdate(obs, new TypeCount).sitAtNew
    	    base.update(obs)

         _tCount += 1
         _oCount+=1
         _pSitAtNew(obs)
       } else {
         val res = _pSitAtOld(obs)
         _oCount+=1
    	 labelTabels(obs).sitAtOld(0, discount)
    	 res
       }
     case MAXPATH =>
       val res = _pSitAtNew(obs)
       	    base.update(obs)

       labelTabels.getOrElseUpdate(obs, new TypeCount).sitAtNew
         _tCount += 1
         _oCount += 1
       res
    } 
  }
  
  def predProb(obs: T) = {
	  _pSitAtOld(obs)+_pSitAtNew(obs)    
  }

  /**
   * 
   * @param obs
   * @return	probability of adding this observation back in
   */
  def remove (obs: T) = {
    val counts = labelTabels(obs)
    _oCount-=1
    val r = (_random.nextDouble*counts.nCust).toInt
    val remCusts = labelTabels(obs).remove(r)
    if (remCusts==0) {
      val pProb = base.remove(obs)
      _tCount-=1
      if (counts.isEmpty)
        labelTabels.remove(obs)
      //_pSitAtNew(obs)
      concentration*pProb / (_oCount+concentration)
    } else {
      (remCusts-discount)/(_oCount+concentration)
    }
  }

  def _logProbSeatingByConc: (Double => Double) = 
    _logProbSeating(_: Double, discount)
  
  def _logProbSeatingByDisc: (Double => Double) =
    _logProbSeating(concentration, _: Double)
    
  /**
   * Sharon's idea (see her and Tom's 2006)
   * we use a Normal-MH sampling, with mean = alpha, and variance = vratio*alpha, just
   * like she did but with a proper prior
   * 
   * Gamma(alpha, beta)
   * 
   */
  def sampleConcentrationMH(shape: Double, rate: Double, vratio: Double=0.01) = {
    val x = concentration
    val y = truncatedNormalVariate(x, vratio*x, 0)
    val lq_xy = math.log(truncatedNormal(x, y, vratio*y,0))
    val lq_yx = math.log(truncatedNormal(y,x,vratio*x,0))
    val lpi_y = _logProbSeatingByConc(y)+Utils.lgammadistShapeRate(y,wordseg.wordseg.shape,wordseg.wordseg.rate)
    val lpi_x = _logProbSeatingByConc(x)+Utils.lgammadistShapeRate(x,wordseg.wordseg.shape,wordseg.wordseg.rate)
    if (_random.nextDouble<((lpi_y+lq_xy)-(lpi_x+lq_yx)))
      concentration = y
    else //reject, keep old value
      concentration = x
  }
    
    
  /**
   * tells you what the normalized probabilites of sitting down are
   */
  def __seatChoice(obs: T) = {
    val pOld = _pSitAtOld(obs)
    val pNew = _pSitAtNew(obs)
    val p = pOld + pNew
    List(("old:",pOld/p),("new:",pNew/p))
  }
  
  }