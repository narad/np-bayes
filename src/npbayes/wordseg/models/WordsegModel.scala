package npbayes.wordseg.models

import java.io.PrintStream
import npbayes.wordseg.data.VarData

abstract class WordsegModel {
	val data: VarData
	def sanity: Boolean
	def init(gold:Boolean = false, goldType:Double=0.5)
	def logProb: Double
	def gibbsSweep(anneal: Double=1.0): Double
	def evaluate: String
	var _logProbTrack: Double = 0 // explicit logProb tracking
	/**
	 * only resample words, but determine drops
	 */
	def gibbsSweepWords(anneal: Double=1.0): Double
	
	def resampleConcentration
	
	def hyperParam: String 
	
	def writeAnalysis(s: PrintStream) =
	  data.printAnalysis(s)

	def writeAnalysisB(s: PrintStream) =
	  data.printAnalysisB(s)
}