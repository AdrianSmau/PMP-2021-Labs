package Lab8

import com.cra.figaro.algorithm.sampling.Importance
import com.cra.figaro.language.{Constant, Element, Select, Flip, Apply}
import com.cra.figaro.library.compound.{If, CPD}
import com.cra.figaro.algorithm.factored.VariableElimination

object Ex3Refined {
  def main(args: Array[String]) {
    val testLength = 10
    val studiedFor: Array[Element[String]] =
      Array.fill(testLength)(Constant(""))
    val passed: Array[Element[Boolean]] =
      Array.fill(testLength)(Constant(false))
    val grade: Array[Element[Int]] =
      Array.fill(testLength)(Constant(0))
    val gradeSum: Array[Element[Int]] =
      Array.fill(testLength)(Constant(0))

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
      grade(exam) = CPD(
        studiedFor(exam),
        passed(exam),
        ("a bit", false) -> Select(0.2 -> 1, 0.4 -> 2, 0.3 -> 3, 0.2 -> 4),
        ("a bit", true) -> Select(
          0.4 -> 5,
          0.3 -> 6,
          0.1 -> 7,
          0.1 -> 8,
          0.07 -> 9,
          0.03 -> 10
        ),
        ("average", false) -> Select(0.1 -> 1, 0.2 -> 2, 0.4 -> 3, 0.3 -> 4),
        ("average", true) -> Select(
          0.1 -> 5,
          0.15 -> 6,
          0.3 -> 7,
          0.15 -> 8,
          0.1 -> 9,
          0.1 -> 10
        ),
        ("much", false) -> Select(0.05 -> 1, 0.15 -> 2, 0.3 -> 3, 0.5 -> 4),
        ("much", true) -> Select(
          0.05 -> 5,
          0.1 -> 6,
          0.15 -> 7,
          0.2 -> 8,
          0.3 -> 9,
          0.2 -> 10
        )
      ) // what grade does the student pass / fail the exam with? it depends on his level of study
      if (exam == 0)
        gradeSum(exam) = grade(exam)
      else
        gradeSum(exam) = Apply(
          gradeSum(exam - 1),
          grade(exam),
          (sum: Int, currGrade: Int) => sum + currGrade
        )
    }
    val gradeSumMean = Apply(gradeSum(testLength - 1), (sum: Int) => (sum / testLength).toDouble)
    // Let's observe and query our model
    // Query 1
    val algorithm = VariableElimination(gradeSumMean)
    algorithm.start()
    algorithm.stop()
    println(
      "The mean value of our student's grade is: " + algorithm.mean(gradeSumMean)
    )


    // Query 2
    studiedFor(0).observe("much")
    studiedFor(1).observe("much")
    studiedFor(2).observe("much")
    val algorithm2 = VariableElimination(gradeSumMean)
    algorithm2.start()
    algorithm2.stop()
    println(
      "The mean value of our student's grade, when he starts the semester by studying for the first 3 exams is: " + algorithm2.mean(gradeSumMean) + ", while, on the last exam, the probability of passing is: " + VariableElimination.probability(passed(9), true)
    )
    studiedFor(0).unobserve()
    studiedFor(1).unobserve()
    studiedFor(2).unobserve()


    // Query 3
    studiedFor(0).observe("a bit")
    studiedFor(1).observe("a bit")
    studiedFor(2).observe("a bit")
    val algorithm3 = VariableElimination(gradeSumMean)
    algorithm3.start()
    algorithm3.stop()
    println(
      "The mean value of our student's grade, when he starts the semester by not studying for the first 3 exams is: " + algorithm3.mean(gradeSumMean) + ", while, on the last exam, the probability of passing is: " + VariableElimination.probability(passed(9), true)
    )
    studiedFor(0).unobserve()
    studiedFor(1).unobserve()
    studiedFor(2).unobserve()


    // Query 4
    passed(9).observe(true)
    passed(8).observe(true)
    passed(7).observe(true)
    val algorithm4 = VariableElimination(gradeSumMean)
    algorithm4.start()
    algorithm4.stop()
    println(
      "The mean value of our student's grade, when he passed the last 3 exams is: " + algorithm4.mean(gradeSumMean) + ", while the probability that he studied for the first exam is: " + VariableElimination.probability(studiedFor(0), "much")
    )
    passed(9).unobserve()
    passed(8).unobserve()
    passed(7).unobserve()


    // Query 5
    passed(9).observe(false)
    passed(8).observe(false)
    val algorithm5 = VariableElimination(gradeSumMean)
    algorithm5.start()
    algorithm5.stop()
    println(
      "The mean value of our student's grade, when he failed the last 2 exams is: " + algorithm5.mean(gradeSumMean) + ", while the probability that he studied (superficially, average) for the 9th exam is: " + VariableElimination.probability(studiedFor(8), "average")
    )
    passed(9).unobserve()
    passed(8).unobserve()
  }
}
