package npbayes.distributions

abstract class Distribution[T] {
  def sample: T = throw new Error("Distribution.sample --- Not implemented")
}

