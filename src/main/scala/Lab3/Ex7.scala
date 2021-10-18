package Lab3

import com.cra.figaro.library.atomic.discrete.{FromRange}
import com.cra.figaro.language.{Flip, Select, Apply, Chain}
import com.cra.figaro.library.compound.If
import com.cra.figaro.algorithm.factored.VariableElimination

object Ex7 {
  def main(args: Array[String]) {
    val numberOfSides =
      Select(0.2 -> 4, 0.2 -> 6, 0.2 -> 8, 0.2 -> 12, 0.2 -> 20)
    val roll = Chain(numberOfSides, ((i: Int) => FromRange(1, i + 1)))
    println(
      "Compute the probability that you rolled a 12-sided die: " + VariableElimination
        .probability(numberOfSides, 12)
    ) //Compute the probability that you rolled a 12-sided die
    println(
      "Compute the probability that you rolled a 7: " + VariableElimination
        .probability(roll, 7)
    ) //Compute the probability that you rolled a 7
    roll.observe(7)
    println(
      "Compute the probability that you rolled a 12-sided die given that you rolled a 7: " + VariableElimination
        .probability(numberOfSides, 12)
    ) //Compute the probability that you rolled a 12-sided die given that you rolled a 7
    roll.unobserve()
    numberOfSides.observe(12)
    println(
      "Compute the probability that you rolled a 7 given that you rolled a 12-sided die: " + VariableElimination
        .probability(roll, 7)
    ) //Compute the probability that you rolled a 7 given that you rolled a 12-sided die
    numberOfSides.unobserve()
  }
}
