package npbayes

abstract case class LexGenerator
case object UNIUNLEARNED extends LexGenerator
case object UNILEARNED extends LexGenerator
case object UNILEARNEDVOWELS extends LexGenerator
case object BIUNLEARNED extends LexGenerator
case object BILEARNED extends LexGenerator
case object BILEARNEDVOWELS extends LexGenerator


package object wordseg {
	val DEBUG = false /** makes everything slow because of checks!! */
	/*
	 * Sharon Goldwater's annealing scheme
	 */
	def annealTemperature(startTemp: Double, annealIters: Int, stopTemp: Double = 1)(iter: Int) = {
	  def inner(iteration: Int) =
	    if (iteration >= annealIters)
	    	 stopTemp
	    else {
	    	val bin: Int = (9*iteration)/annealIters+1
      		(10.0/bin-1)*(startTemp-stopTemp)/9.0 + stopTemp
	    }
      inner(iter)
	}
	
	def annealTemperature(iter: Int) = {
	  def inner(iteration: Int) =
	    if (iteration >= 7000)
	    	 1
	    else {
	    	val bin: Int = (9*iteration)/7000+1
      		(10.0/bin-1)*(10-1)/9.0 + 1
	    }
      inner(iter)
	}
}