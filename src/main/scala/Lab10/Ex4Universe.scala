package Lab10

import com.cra.figaro.algorithm.sampling.Importance
import com.cra.figaro.language.{
  Universe,
  Constant,
  Element,
  Select,
  Flip,
  Apply,
  Chain
}
import com.cra.figaro.library.compound.{If, CPD, ^^}
import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.algorithm.filtering.ParticleFilter

object Ex4Universe {
  val initial = Universe.createNew()
  val startingCapitalBudget = 1000000.0
  val investmentPolicyPercent = 25.0
  Constant(startingCapitalBudget)("capital", initial)
  Constant(investmentPolicyPercent)("policy", initial)

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

  Constant(risk)("risk", initial)

  val initialInvestment = Apply(
    Constant(startingCapitalBudget),
    Constant(investmentPolicyPercent),
    (capital: Double, policy: Double) => (capital * policy) / 100.0
  )

  Apply(
    Constant(startingCapitalBudget),
    Constant(investmentPolicyPercent),
    (capital: Double, policy: Double) => (capital * policy) / 100.0
  )("investment", initial)

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

  Apply(
    initialInvestment,
    profitMultiplyer,
    (inv: Double, multiplier: Double) => inv * multiplier
  )("profit", initial)

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

  def nextUniverse(previous: Universe): Universe = {
    val next = Universe.createNew()
    val policy = previous.get[Double]("policy")
    val risk = previous.get[String]("risk")
    val prevInvestment = previous.get[Double]("investment")
    val prevProfit = previous.get[Double]("profit")
    val prevCapital = previous.get[Double]("capital")
    val newState = Chain(
      ^^(prevInvestment, prevProfit, prevCapital, policy, risk),
      (touple: (Double, Double, Double, Double, String)) =>
        stateTransition(touple._1, touple._2, touple._3, touple._4, touple._5)
    )
    Apply(newState, (s: (Double, Double, Double)) => s._1)("investment", next)
    Apply(newState, (s: (Double, Double, Double)) => s._2)("profit", next)
    Apply(newState, (s: (Double, Double, Double)) => s._3)("capital", next)
    Apply(policy, (p : Double) => p)("policy", next)
    Apply(risk, (r : String) => r)("risk", next)
    next
  }
  def main(args: Array[String]) {
    val alg = ParticleFilter(initial, nextUniverse, 10000)
    alg.start()
    for { time <- 1 to 10 } {
      alg.advanceTime()
      print("Time " + time + ": ")
      println(
        "Expected Profit = " + alg.currentExpectation(
          "profit",
          (p: Double) => p
        )
      )
      println(
        "Expected Investment = " + alg.currentExpectation(
          "investment",
          (p: Double) => p
        )
      )
      println(
        "Expected Capital = " + alg.currentExpectation(
          "capital",
          (p: Double) => p
        )
      )
    }
  }
}
