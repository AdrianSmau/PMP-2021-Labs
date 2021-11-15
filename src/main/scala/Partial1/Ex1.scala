package Partial1

import com.cra.figaro.library.atomic.discrete.{FromRange}
import com.cra.figaro.language.{Constant, Flip, Select, Apply}
import com.cra.figaro.library.compound.{RichCPD, OneOf, If, *}
import com.cra.figaro.algorithm.factored.VariableElimination

object Ex1 {
  def main(args: Array[String]) {
    val die1P1 = FromRange(1, 7) // modelam primul zar aruncat de primul jucator
    val die2P1 =
      FromRange(1, 7) // modelam al doilea zar aruncat de primul jucator
    val die1P2 =
      FromRange(1, 7) // modelam primul zar aruncat de al doilea jucator
    val die2P2 =
      FromRange(1, 7) // modelam al doilea zar aruncat de al doilea jucator
    val p1Sum = Apply(
      die1P1,
      die2P1,
      (i1: Int, i2: Int) => i1 + i2
    ) // calculam suma zarurilor primului jucator
    val p2Sum = Apply(
      die1P2,
      die2P2,
      (i1: Int, i2: Int) => i1 + i2
    ) // calculam suma zarurilor celui de-al doilea jucator
    // facem, cu ajutorul unui RichCPD, modelarea situatiilor de castig sau remiza conform enuntului, returnand variabile de tip Constant
    // Posibilitatea ca P1 sa castige
    val p1Winner = RichCPD(
      p1Sum,
      OneOf(7, 11) -> Constant(true),
      * -> Constant(false)
    )

    // Posibilitatea ca P2 sa castige
    val p2Winner = RichCPD(
      p2Sum,
      OneOf(2, 3, 12) -> Constant(true),
      * -> Constant(false)
    )

    // Posibilitatea sa fie remiza
    val isDraw = If(p1Winner, false, If(p2Winner, false, true))

    // Cerinta 2
    println(VariableElimination.probability(p1Winner, true))
  }
}
