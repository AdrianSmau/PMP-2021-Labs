package Lab3

import com.cra.figaro.library.atomic.discrete.{FromRange}
import com.cra.figaro.language.{Flip, Select, Apply}
import com.cra.figaro.library.compound.If
import com.cra.figaro.algorithm.factored.VariableElimination

object Ex6 {
  def main(args: Array[String]) {
    val die1 = FromRange(1, 7)
    val die2 = FromRange(1, 7)
    val die3 = FromRange(1, 7)
    val die4 = FromRange(1, 7)
    val die5 = FromRange(1, 7)
    val die6 = FromRange(1, 7)
    val jail =
      (die1 === die2) && (die3 === die4) && (die5 === die6) && (die1 !== die3) && (die3 !== die5) && (die1 !== die5) // Am impus conditia ca dublele sa fie diferite una de cealalta
    println(VariableElimination.probability(jail, true))
  }
}
