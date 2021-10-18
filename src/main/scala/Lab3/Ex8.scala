package Lab3

import com.cra.figaro.library.atomic.discrete.{FromRange}
import com.cra.figaro.language.{Flip, Select, Apply, Chain}
import com.cra.figaro.library.compound.{If, ^^}
import com.cra.figaro.algorithm.factored.VariableElimination

object Ex8 {
  def main(args: Array[String]) {
    val numberOfSides1 =
      Select(0.2 -> 4, 0.2 -> 6, 0.2 -> 8, 0.2 -> 12, 0.2 -> 20)
    val roll1 = Chain(numberOfSides1, ((i: Int) => FromRange(1, i + 1)))
    val numberOfSides2 =
      Select(0.2 -> 4, 0.2 -> 6, 0.2 -> 8, 0.2 -> 12, 0.2 -> 20)
    val roll2 = Chain(numberOfSides2, ((i: Int) => FromRange(1, i + 1)))
    def stickinessConstraint(sidesPair: (Int, Int)) =
      if (sidesPair._1 == sidesPair._2) 1.0 else 0.5
    val pairOfSides = ^^(numberOfSides1, numberOfSides2)
    pairOfSides.addConstraint(stickinessConstraint)
    println(
      VariableElimination.probability(roll2, 7)
    ) // prints 0.05166666666666667
    roll1.observe(7)
    println(
      VariableElimination.probability(roll2, 7)
    ) // prints 0.09704301075268815
    roll1.unobserve()
    roll1.observe(1)
    println(
      VariableElimination.probability(roll2, 11)
    ) //Compute the probability that you rolled a 11 on the second roll given that you rolled a 1 on the first roll.
    roll1.unobserve()
    roll2.observe(10)
    println(
      VariableElimination.probability(roll1, 10)
    ) //Compute the probability that you rolled a 10 on the first roll given that you rolled a 10 on the second roll.
    roll2.unobserve()
  }
}
