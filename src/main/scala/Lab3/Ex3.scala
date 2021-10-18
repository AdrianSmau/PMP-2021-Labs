package Lab3

import com.cra.figaro.language.{Flip, Select}
import com.cra.figaro.library.compound.If
import com.cra.figaro.algorithm.factored.VariableElimination

object Ex3a {
  def main(args: Array[String]) {
    val x = Flip(0.4)
    val y = Flip(0.4)
    val z = x
    val w = x === z
    println(VariableElimination.probability(w, true))
    //1.0
  }
}
object Ex3b {
  def main(args: Array[String]) {
    val x = Flip(0.4)
    val y = Flip(0.4)
    val z = y
    val w = x === z
    println(VariableElimination.probability(w, true))
    //0.52
  }
}
