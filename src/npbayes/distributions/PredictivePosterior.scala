package npbayes.distributions

trait PosteriorPredictive[T] extends Distribution[T] {
  def logProb: Double = 0
  def apply(obs: T) = predProb(obs: T)
  def predProb (obs: T): Double
  def update (obs: T): Double
  def remove (obs: T): Double
}
