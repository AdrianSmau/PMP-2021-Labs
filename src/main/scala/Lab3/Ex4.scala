package Lab3

import com.cra.figaro.library.atomic.discrete.{FromRange}
import com.cra.figaro.language.{Flip, Select, Apply}
import com.cra.figaro.library.compound.If
import com.cra.figaro.algorithm.factored.VariableElimination

object Ex4 {
  def main(args: Array[String]) {
    val die1 = FromRange(1, 7)
    val die2 = FromRange(1, 7)
    val die3 = FromRange(1, 7)
    val die4 = FromRange(1, 7)
    val die5 = FromRange(1, 7)
    //val die6 = FromRange(1, 7)
    val total = Apply(
      die1,
      die2,
      die3,
      die4,
      die5 /*, die6*/,
      (i1: Int, i2: Int, i3: Int, i4: Int, i5: Int /*, i6: Int*/ ) =>
        i1 + i2 + i3 + i4 + i5 /* + i6*/
    )
    println(
      VariableElimination.probability(total, 11)
    ) // Varianta cu 3 zaruri genereaza o probabilitate de 0.125
  }
}
