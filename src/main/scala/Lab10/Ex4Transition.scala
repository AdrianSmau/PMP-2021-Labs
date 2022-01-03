package Lab10

import com.cra.figaro.algorithm.sampling.Importance
import com.cra.figaro.language.{Constant, Element, Select, Flip, Apply, Chain}
import com.cra.figaro.library.compound.{If, CPD, ^^}
import com.cra.figaro.algorithm.factored.VariableElimination

object Ex4Transition {
  def stateTransition(
      prevInvestment: Double,
      prevProfit: Double,
      prevCapital: Double,
      investmentPolicyPercent: Double,
      risk: String
  ): Element[(Double, Double, Double)] = {
    var currentProfitMultiplyer: Element[Double] = Constant(0.0)
    val newInvestment = Apply(
      Constant(prevCapital),
      Constant(investmentPolicyPercent),
      (prevCapital: Double, policy: Double) => (prevCapital * policy) / 100.0
    )
    if (risk == "low") {
      currentProfitMultiplyer =
        Select(0.7 -> 1.15, 0.3 -> 1.2) // low risk, low reward
    } else {
      if (risk == "medium") {
        currentProfitMultiplyer =
          Select(0.3 -> 1.2, 0.3 -> 1.35, 0.3 -> 0.75) // middle way
      } else {
        currentProfitMultiplyer = Select(
          0.2 -> 1.05,
          0.4 -> 0.5,
          0.4 -> 1.5
        ) // high risk, high reward
      }
    }
    val newProfit = newInvestment * currentProfitMultiplyer
    val newCapital = Apply(
      Constant(prevCapital),
      newProfit,
      newInvestment,
      (
          prevCapital: Double,
          currentProfit: Double,
          currentInvestment: Double
      ) => prevCapital + currentProfit - currentInvestment
    )
    ^^(newInvestment, newProfit, newCapital)
  }
  def firmEconomy(
      iterationNum: Int,
      startingCapitalBudget: Double,
      investmentPolicyPercent: Double
  ): Element[Double] = {
    val investment: Array[Element[Double]] =
      Array.fill(iterationNum)(Constant(0))
    val profit: Array[Element[Double]] =
      Array.fill(iterationNum)(Constant(0))
    val capital: Array[Element[Double]] = Array.fill(iterationNum)(Constant(0))

    var risk: String = "undefined"
    var profitMultiplyer: Element[Double] = Constant(0.0)

    if (investmentPolicyPercent < 15) {
      risk = "low"
    } else {
      if (investmentPolicyPercent < 30) {
        risk = "medium"
      } else {
        risk = "high"
      }
    }

    capital(0) = Constant(startingCapitalBudget)
    investment(0) = Apply(
      capital(0),
      Constant(investmentPolicyPercent),
      (prevCapital: Double, policy: Double) => (prevCapital * policy) / 100.0
    )

    if (risk == "low") {
      profitMultiplyer = Select(0.7 -> 1.15, 0.3 -> 1.2) // low risk, low reward
    } else {
      if (risk == "medium") {
        profitMultiplyer =
          Select(0.3 -> 1.2, 0.3 -> 1.35, 0.3 -> 0.75) // middle way
      } else {
        profitMultiplyer =
          Select(0.2 -> 1.05, 0.4 -> 0.5, 0.4 -> 1.5) // high risk, high reward
      }
    }
    profit(0) = investment(0) * profitMultiplyer

    for { i <- 1 until iterationNum } {
      val newState = Chain(
        ^^(investment(i - 1), profit(i - 1), capital(i - 1)),
        (paramTouple: (Double, Double, Double)) =>
          stateTransition(
            paramTouple._1,
            paramTouple._2,
            paramTouple._3,
            investmentPolicyPercent,
            risk
          )
      )
      investment(i) = newState._1
      profit(i) = newState._2
      capital(i) = newState._3
    }
    capital(iterationNum - 1)
  }
  def main(args: Array[String]) {
    //Query 1
    val query1 = firmEconomy(10, 1000000, 10)
    val algorithm1 = Importance(2000, query1)
    algorithm1.start()
    algorithm1.stop()
    println(
      "[10%] After 10 iterations, the capital of a firm with 1 million$ starting capital that chooses to invest 10% will be: " + algorithm1
        .mean(query1)
    )

    //Query 2
    val query2 = firmEconomy(10, 1000000, 15)
    val algorithm2 = Importance(2000, query2)
    algorithm2.start()
    algorithm2.stop()
    println(
      "[15%] After 10 iterations, the capital of a firm with 1 million$ starting capital that chooses to invest 15% will be: " + algorithm2
        .mean(query2)
    )

    //Query 3
    val query3 = firmEconomy(10, 1000000, 20)
    val algorithm3 = Importance(2000, query3)
    algorithm3.start()
    algorithm3.stop()
    println(
      "[20%] After 10 iterations, the capital of a firm with 1 million$ starting capital that chooses to invest 20% will be: " + algorithm3
        .mean(query3)
    )

    //Query 4
    val query4 = firmEconomy(10, 1000000, 25)
    val algorithm4 = Importance(2000, query4)
    algorithm4.start()
    algorithm4.stop()
    println(
      "[25%] After 10 iterations, the capital of a firm with 1 million$ starting capital that chooses to invest 25% will be: " + algorithm4
        .mean(query4)
    )

    //Query 5
    val query5 = firmEconomy(10, 1000000, 30)
    val algorithm5 = Importance(2000, query5)
    algorithm5.start()
    algorithm5.stop()
    println(
      "[30%] After 10 iterations, the capital of a firm with 1 million$ starting capital that chooses to invest 30% will be: " + algorithm5
        .mean(query5)
    )

    //Query 6
    val query6 = firmEconomy(10, 1000000, 35)
    val algorithm6 = Importance(2000, query6)
    algorithm6.start()
    algorithm6.stop()
    println(
      "[35%] After 10 iterations, the capital of a firm with 1 million$ starting capital that chooses to invest 35% will be: " + algorithm6
        .mean(query6)
    )

    //Query 7
    val query7 = firmEconomy(10, 1000000, 40)
    val algorithm7 = Importance(2000, query7)
    algorithm7.start()
    algorithm7.stop()
    println(
      "[40%] After 10 iterations, the capital of a firm with 1 million$ starting capital that chooses to invest 40% will be: " + algorithm7
        .mean(query7)
    )

    //Query 8
    val query8 = firmEconomy(10, 1000000, 45)
    val algorithm8 = Importance(2000, query8)
    algorithm8.start()
    algorithm8.stop()
    println(
      "[45%] After 10 iterations, the capital of a firm with 1 million$ starting capital that chooses to invest 45% will be: " + algorithm8
        .mean(query8)
    )

    //Query 9
    val query9 = firmEconomy(10, 1000000, 50)
    val algorithm9 = Importance(2000, query9)
    algorithm9.start()
    algorithm9.stop()
    println(
      "[50%] After 10 iterations, the capital of a firm with 1 million$ starting capital that chooses to invest 50% will be: " + algorithm9
        .mean(query9)
    )
  }
}
