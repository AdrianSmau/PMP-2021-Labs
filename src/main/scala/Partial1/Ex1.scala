package Partial1

import com.cra.figaro.library.atomic.discrete.{FromRange}
import com.cra.figaro.language.{Element, Constant, Flip, Select, Apply}
import com.cra.figaro.library.compound.{RichCPD, OneOf, If, *}
import com.cra.figaro.algorithm.factored.VariableElimination

object Ex1 {
  def play(
      p1Wins: Element[Int],
      p2Wins: Element[Int],
      no: Int
  ): Element[Int] = {
    if (no == 0) { // modelam cazul de oprire
      If(
        Apply(
          p1Wins,
          p2Wins,
          (p1Wins: Int, p2Wins: Int) => p1Wins > p2Wins
        ),
        Constant(1),
        If(Apply(
          p1Wins,
          p2Wins,
          (p1Wins: Int, p2Wins: Int) => p1Wins < p2Wins
        ), Constant(2), Constant(0))
      )
    }
    // remodelam jocul din main
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

    if (p1Winner == Constant(true))
      play(
        Apply(
          p1Wins,
          (wins: Int) => wins + 1
        ),
        p2Wins,
        (no - 1)
      ) // player 1 a castigat, reapelam

    // Posibilitatea ca P2 sa castige
    val p2Winner = RichCPD(
      p2Sum,
      OneOf(2, 3, 12) -> Constant(true),
      * -> Constant(false)
    )

    if (p2Winner == Constant(true))
      play(
        p1Wins,
        Apply(
          p2Wins,
          (wins: Int) => wins + 1
        ),
        (no - 1)
      ) // player 2 a castigat, reapelam functia

    play(p1Wins, p2Wins, (no - 1)) // cazul de fallback, sigur e remiza
  }

  def main(args: Array[String]) {
    // Cerinta 1
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
    // Cerinta 3
    println(VariableElimination.probability(p2Winner, true))
    // Cerinta 4
    // Cerinta 5 - functia modelata mai sus
    // Cerinta 6
    //Trebuie sa simulam jocul de 10 ori si sa numaram de cate ori castiga fiecare
    /*val simulation = Array.fill(10)(
      play(Constant(0), Constant(0), 1)
    )*/ // jucam jocul de 10 ori, acum tot  ce trebuie sa facem este sa numaram cate instante de 1,2 sau 0 avem
    // SAU
    
    // Cu ajutorul functiei Importance, cu primul argument 10, deci bazat pe esantionare...
  }
}
