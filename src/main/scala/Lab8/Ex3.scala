package Lab8

import com.cra.figaro.language.{Constant, Element, Select, Flip, Apply}
import com.cra.figaro.library.compound.{If, CPD}
import com.cra.figaro.algorithm.factored.VariableElimination

object Ex3 {
  def main(args: Array[String]) {
    val testLength = 10
    val studiedFor: Array[Element[String]] =
      Array.fill(testLength)(Constant(""))
    val passed: Array[Element[Boolean]] =
      Array.fill(testLength)(Constant(false))

    for { exam <- 0 until testLength } {
      if (exam == 0) {
        studiedFor(exam) = Select(
          0.3 -> "a bit",
          0.3 -> "average",
          0.3 -> "much"
        ) // the 3 cases of study levels are being Selected with the same possibility initially
      } else {
        studiedFor(exam) = CPD(
          studiedFor(exam - 1),
          "a bit" -> Select(0.65 -> "a bit", 0.2 -> "average", 0.15 -> "much"),
          "average" -> Select(0.3 -> "a bit", 0.5 -> "average", 0.2 -> "much"),
          "much" -> Select(0.2 -> "a bit", 0.45 -> "average", 0.55 -> "much")
        ) // the level of study of the student for the current state depends on the current level of study. So, if the student learned a bit for the previous state, chances are he will learn a bit for the current state too. Same goes with all the 2 other cases
      }
      passed(exam) = CPD(
        studiedFor(exam),
        "a bit" -> Flip(0.3),
        "average" -> Flip(0.5),
        "much" -> Flip(0.8)
      ) // does the student pass the exam? based on the level of study he did
    }
    // Let's observe the fact that the student passed the first 3 exams
    passed(0).observe(true)
    passed(1).observe(true)
    passed(2).observe(true)
    println("The probability that the student passes the last test given the fact that he passed the first 3 is: " + VariableElimination.probability(passed(9), true))
  }
}
