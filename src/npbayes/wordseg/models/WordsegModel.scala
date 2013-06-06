package npbayes.wordseg.models

import java.io.PrintStream
import npbayes.wordseg.data.Data

abstract class WordsegModel {
	val data: Data
	def sanity: Boolean = false
	def init(gold:Boolean = false, goldType:Double=0.5) = {}
	def logProb: Double = 0.0
	def gibbsSweep(anneal: Double=1.0): Double = 0.0
	def evaluate: String = data.evaluate.toString
	var _logProbTrack: Double = 0 // explicit logProb tracking
	/**
	 * only resample words, but determine drops
	 */
	def gibbsSweepWords(anneal: Double=1.0): Double = 0.0
	
	def resampleConcentration(hsiters: Int=1): Unit = {}
	
	def optimizeConcentration: Unit = {}

	
	def hyperParam: String = ""
	
	def writeAnalysis(s: PrintStream) =
	  data.printAnalysis(s)

	def writeAnalysisB(s: PrintStream) =
	  data.printAnalysisB(s)
}