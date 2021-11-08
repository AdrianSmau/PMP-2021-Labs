package Lab6

import com.cra.figaro.library.atomic.continuous
import com.cra.figaro.language.{Constant, Select, Chain, Element, Apply}
import com.cra.figaro.library.atomic.discrete.{Uniform}
import com.cra.figaro.algorithm.sampling.Importance
import com.cra.figaro.library.collection.Container
import com.cra.figaro.algorithm.factored.VariableElimination

object Ex3 {
  def main(args: Array[String]) {
    val skillLevelDistribution = continuous.Uniform(0.0, 8 / 13)
    val start = 3
    val end = 7
    val rand = new scala.util.Random
    val pars = Array.fill(18)(start + rand.nextInt((end - start) + 1))
    val generatedHitsPerHole = for {
      i <- 0 until pars.length
    } yield Chain(
      skillLevelDistribution,
      (distribution: Double) =>
        Select(
          distribution / 8 -> (pars(i) - 2),
          distribution / 2 -> (pars(i) - 1),
          distribution -> pars(i),
          4 / 5 * (1 - 13 * distribution / 8) -> (pars(i) + 1),
          1 / 5 * (1 - 13 * distribution / 8) -> (pars(i) + 2)
        )
    )

	val sumOfHits = Container(generatedHitsPerHole:_*).foldLeft(0)(_ + _).map(_.toInt)

	/*def greaterThan80(hitSum: Int) =
      if (hitSum > 80) hitSum else 0
	sumOfHits.addConstraint(greaterThan80);*/

	//Instructiunea de mai jos genereaza ZeroTotalUnnormalizedProbabilityException, insa modelul compileaza. Nu pot intreba modelul cerintele exercitiului.

	//println(VariableElimination.probability(sumOfHits, 80))
  }
}
