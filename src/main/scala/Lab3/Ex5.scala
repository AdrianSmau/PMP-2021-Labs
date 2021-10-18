package Lab3

import com.cra.figaro.library.atomic.discrete.{FromRange}
import com.cra.figaro.language.{Flip, Select, Apply}
import com.cra.figaro.library.compound.If
import com.cra.figaro.algorithm.factored.VariableElimination

object Ex5a {
  def main(args: Array[String]) {
    val die1 = FromRange(1, 7)
    val die2 = FromRange(1, 7)
    val total = Apply(die1, die2, (i1: Int, i2: Int) => i1 + i2)
    total.addCondition((i: Int) => i < 10)
    println(
      VariableElimination.probability(die1, 5)
    ) // Probabilitatea ca, aruncand 2 zaruri, unul dintre ele sa aiba valoarea 5 si suma lor sa fie mai mica decat 10
  }
}

object Ex5b {
  def main(args: Array[String]) {
    val die1 = FromRange(1, 7)
    val die2 = FromRange(1, 7)
    val die3 = FromRange(1, 7)
    val total =
      Apply(die1, die2, die3, (i1: Int, i2: Int, i3: Int) => i1 + i2 + i3)
    total.addCondition((i: Int) => i > 11)
    println(
      VariableElimination.probability(die1, 1)
    ) // Probabilitatea ca, aruncand 3 zaruri, unul dintre ele sa aiba valoarea 1 si suma lor sa fie mai mare decat 11
  }
}

object Ex5c {
  def main(args: Array[String]) {
    val die1 = FromRange(1, 7)
    val die2 = FromRange(1, 7)
    val total = Apply(die1, die2, (i1: Int, i2: Int) => i1 + i2)
    total.addCondition((i: Int) => i < 5)
    die1.observe(2)
    println(
      VariableElimination.probability(die2, 2)
    ) // Probabilitatea ca, aruncand 2 zaruri, unul dintre ele sa aiba valoarea 2 atunci cand celalalt are valoarea 2 si suma lor sa fie mai mice decat 5
  }
}
