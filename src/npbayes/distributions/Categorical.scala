package npbayes.distributions

/**
 * a categorical distribution that is easy to sample from
 * 
 * add outcomes using add, you can pass unnormalized probabilities
 * 
 * use sample to produce an outcome of type T
 */
class Categorical[T] {
  var outcomes: List[(T,Double)] = List.empty
  var partition: Double = 0
  
  def add(event: T, prob: Double): Unit = {
    outcomes = outcomes:+((event,prob))
    partition += prob
  }
  
  override def toString = {
    var res=""
    for ((out,prob) <- outcomes)
      res+=out+" "+prob/partition+"\n"
    res
  }
  
  def sample: T =
    Categorical.sample(outcomes, partition)(Categorical.unif.nextDouble)
  
  
  def sample(anneal: Double): T = {
    val annealed = outcomes.map(x=>(x._1,math.pow(x._2, anneal)))
    val part = annealed.map(x=>x._2).sum
    Categorical.sample(annealed,part)(Categorical.unif.nextDouble)
  }
}

object Categorical {
  val unif = new scala.util.Random()
  
  def sample[T](outcomes: List[(T,Double)],partition: Double)(rnd: Double) = {
    def inner(events: List[(T,Double)],cur: Double,flip: Double): T = events match {
      case List() => throw new Error("Categorical.sample: couldn't produce sample")
      case (res,prob)::tail => {
        if (flip<=cur+prob)
          res
        else
          inner(tail,cur+prob,flip)
      }
    }
    inner(outcomes,0,rnd*partition)
  }
}
