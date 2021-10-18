package Lab3

import com.cra.figaro.language.{Flip, Select}
import com.cra.figaro.library.compound.If
import com.cra.figaro.algorithm.factored.VariableElimination

object Ex1 {
  def main(args: Array[String]) {
    val sunnyToday = Flip(0.2)
    val greetingToday = If(
      sunnyToday,
      Select(0.6 -> "Hello, world!", 0.4 -> "Howdy, universe!"),
      Select(0.2 -> "Hello, world!", 0.8 -> "Oh no, not again")
    )

    val sideOfBed = Flip(
      0.3
    ) // Probabilitate 30% de a te trezi pe partea dreapta
    val amICheerful = If(
      sideOfBed,
      Select(0.6 -> "Hello, world!", 0.4 -> "Howdy, universe!"),
      "Oh no, not again"
    )
  }
}
