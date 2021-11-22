package Lab7

import com.cra.figaro.algorithm.sampling.Importance
import com.cra.figaro.library.atomic.continuous.{Uniform}
import com.cra.figaro.language.{Element, Select, Flip, Apply, Chain}
import com.cra.figaro.library.compound.{If, ^^}
import com.cra.figaro.algorithm.factored.VariableElimination

object Departments {

  class ResearchAndDevelopment {
    val value = Uniform(20, 90) // rd can have any value from 20 to 90
  }

  class HumanResources {
    val value = Uniform(20, 90) // same with hr
  }

  class Production(val rd: ResearchAndDevelopment, val hr: HumanResources) {
    // Let's say x = arithmetic average of rd and hr
    val value = Chain(
      ^^(rd.value, hr.value),
      (touple: (Double, Double)) =>
        if (touple._1 > 70) {
          if (
            touple._2 > 70
          ) // if both rd and hr scores are above 70, 90% -> x, 10% -> x - 5
            Select(
              0.9 -> (touple._1 + touple._2) / 2,
              0.1 -> ((touple._1 + touple._2) / 2 - 5)
            )
          else // if only rd score is above 70, 70% -> x, 30% -> x - 5
            Select(
              0.7 -> (touple._1 + touple._2) / 2,
              0.3 -> ((touple._1 + touple._2) / 2 - 5)
            )
        } else {
          if (
            touple._2 > 70
          ) // if only hr score is above 70, 70% -> x, 30% -> x - 5
            Select(
              0.7 -> (touple._1 + touple._2) / 2,
              0.3 -> ((touple._1 + touple._2) / 2 - 5)
            )
          else // if both rd and hr scores are below 70, 60% -> x, 40% -> x - 5
            Select(
              0.4 -> (touple._1 + touple._2) / 2,
              0.6 -> ((touple._1 + touple._2) / 2 - 5)
            )
        }
    )
  }

  class Sales(val p: Production) {
    val value = Apply(
      p.value,
      (production: Double) =>
        if (production > 80)
          production + 5 // if production is good, sales are even better
        else {
          if (production > 50) production
          else
            production - 5 // if production is average, so are the sales, and if the production is poor, sales are even worse
        }
    )
  }

  class Finance(val hr: HumanResources, val s: Sales) {
    // Let's say x = arithmetic average of hr and s
    // WE APPLY THE SAME LOGIC AS WE APPLIED ON PRODUCTION
    val value = Chain(
      ^^(hr.value, s.value),
      (touple: (Double, Double)) =>
        if (touple._1 > 70) {
          if (
            touple._2 > 70
          ) // if both hr and s scores are above 70, 90% -> x, 10% -> x - 5
            Select(
              0.9 -> (touple._1 + touple._2) / 2,
              0.1 -> ((touple._1 + touple._2) / 2 - 5)
            )
          else // if only hr score is above 70, 70% -> x, 30% -> x - 5
            Select(
              0.7 -> (touple._1 + touple._2) / 2,
              0.3 -> ((touple._1 + touple._2) / 2 - 5)
            )
        } else {
          if (
            touple._2 > 70
          ) // if only s score is above 70, 70% -> x, 30% -> x - 5
            Select(
              0.7 -> (touple._1 + touple._2) / 2,
              0.3 -> ((touple._1 + touple._2) / 2 - 5)
            )
          else // if both hr and s scores are below 70, 60% -> x, 40% -> x - 5
            Select(
              0.4 -> (touple._1 + touple._2) / 2,
              0.6 -> ((touple._1 + touple._2) / 2 - 5)
            )
        }
    )
  }

  class Firm(
      val rd: ResearchAndDevelopment,
      val hr: HumanResources,
      val p: Production,
      val s: Sales,
      val f: Finance
  ) {
    val health =
      Apply( // We need to make this value Element[Double] in order to compute its mean value
        ^^(rd.value, hr.value, p.value, s.value, f.value),
        (touple: (Double, Double, Double, Double, Double)) =>
          if (
            (touple._1 + touple._2 + touple._3 + touple._4 + touple._5) > 450.0 && (touple._1 + touple._2 + touple._3 + touple._4 + touple._5) < 475.0
          )
            (touple._1 + touple._2 + touple._3 + touple._4 + touple._5 + 20.0) / 5.0 // Bonus 20 if everything is outstanding
          else {
            if (
              (touple._1 + touple._2 + touple._3 + touple._4 + touple._5) > 400.0
            )
              (touple._1 + touple._2 + touple._3 + touple._4 + touple._5 + 15.0) / 5.0 // Bonus 15 if everything is great
            else {
              if (
                (touple._1 + touple._2 + touple._3 + touple._4 + touple._5) > 350.0
              )
                (touple._1 + touple._2 + touple._3 + touple._4 + touple._5 + 10.0) / 5.0 // Bonus 10 if everything is good
              else {
                if (
                  (touple._1 + touple._2 + touple._3 + touple._4 + touple._5) > 300.0
                )
                  (touple._1 + touple._2 + touple._3 + touple._4 + touple._5) / 5.0 // No bonus if everything is OK
                else {
                  if (
                    (touple._1 + touple._2 + touple._3 + touple._4 + touple._5) > 200.0
                  )
                    (touple._1 + touple._2 + touple._3 + touple._4 + touple._5 - 10.0) / 5.0 // Penalty 10 if everything is not so good
                  else {
                    if (
                      (touple._1 + touple._2 + touple._3 + touple._4 + touple._5) > 150.0
                    )
                      (touple._1 + touple._2 + touple._3 + touple._4 + touple._5 - 15.0) / 5.0 // Penalty 15 if everything is bad
                    else {
                      if (
                        (touple._1 + touple._2 + touple._3 + touple._4 + touple._5) > 25.0
                      )
                        (touple._1 + touple._2 + touple._3 + touple._4 + touple._5 - 20.0) / 5.0 // Penalty 20 if everything is poor
                      else
                        (touple._1 + touple._2 + touple._3 + touple._4 + touple._5) / 5.0 // fallback case
                    }
                  }
                }
              }
            }
          }
      )
  }

  def main(args: Array[String]) {
    val rd = new ResearchAndDevelopment()
    val hr = new HumanResources()
    val p = new Production(rd, hr)
    val s = new Sales(p)
    val f = new Finance(hr, s)
    val firm = new Firm(rd, hr, p, s, f)

    val algorithm = Importance(1000, firm.health)
    algorithm.start()
    algorithm.stop()
    println(
      "The mean value of Firm Health without any constraints is: " + algorithm
        .mean(firm.health)
    )

    // We observe some specific values for hr and rd

    rd.value.observe(85)
    hr.value.observe(85)
    val algorithm2 = Importance(1000, firm.health)
    algorithm2.start()
    algorithm2.stop()
    println(
      "The mean value of Firm Health when rd and hr values are high is: " + algorithm2
        .mean(firm.health)
    )
    rd.value.unobserve()
    hr.value.unobserve()

    rd.value.observe(20)
    hr.value.observe(20)
    val algorithm3 = Importance(1000, firm.health)
    algorithm3.start()
    algorithm3.stop()
    println(
      "The mean value of Firm Health when rd and hr values are minimum is: " + algorithm3
        .mean(firm.health)
    )
    rd.value.unobserve()
    hr.value.unobserve()

    rd.value.observe(45)
    hr.value.observe(60)
    val algorithm4 = Importance(1000, firm.health)
    algorithm4.start()
    algorithm4.stop()
    println(
      "The mean value of Firm Health when rd and hr values are average is: " + algorithm4
        .mean(firm.health)
    )
    rd.value.unobserve()
    hr.value.unobserve()
  }
}
