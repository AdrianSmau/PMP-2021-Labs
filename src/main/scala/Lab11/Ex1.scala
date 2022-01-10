package Lab11

import com.cra.figaro.language.Flip
import com.cra.figaro.library.compound.CPD
import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.algorithm.sampling.Importance
import com.cra.figaro.algorithm.factored.beliefpropagation.BeliefPropagation

object Ex1 {
  def main(args: Array[String]) {
    // a)
    var isPresident = Flip(2.5e-8)
    var isLeftHanded = CPD(isPresident, true -> Flip(0.5), false -> Flip(0.1))
    isLeftHanded.observe(true)

    println("--- A ---")
    println("VE: " + VariableElimination.probability(isPresident, true))

    val algorithmAImp = Importance(10000, isPresident)
    algorithmAImp.start()
    println("Importance: " + algorithmAImp.probability(isPresident, true))
    algorithmAImp.kill()

    val algorithmABP = BeliefPropagation(100, isPresident)
    algorithmABP.start()
    println("BP: " + algorithmABP.probability(isPresident, true))
    algorithmABP.kill()
    isLeftHanded.unobserve()

    // b)
    var wentToHarvard =
      CPD(isPresident, true -> Flip(0.15), false -> Flip(0.0005))
    wentToHarvard.observe(true)

    println("\n--- B ---")
    println("VE: " + VariableElimination.probability(isPresident, true))

    val algorithmBImp = Importance(10000, isPresident)
    algorithmBImp.start()
    println("Importance: " + algorithmBImp.probability(isPresident, true))
    algorithmBImp.kill()

    val algorithmBBP = BeliefPropagation(100, isPresident)
    algorithmBBP.start()
    println("BP: " + algorithmBBP.probability(isPresident, true))
    algorithmBBP.kill()
    wentToHarvard.unobserve()

    // c) - Supposing that they are conditionally independent, we can compute that P(leftHanded, Harvard | President) = P(leftHanded | President) * P(Harvard | President) = 0.5 * 0.15 = 0.075
    // and P(leftHanded, Harvard | !President) = P(leftHanded | !President) * P(Harvard | !President) = 0.1 * 0.0005 = 0.00005
    var isLeftHandedAndWentToHarvard =
      CPD(isPresident, true -> Flip(0.075), false -> Flip(0.00005))
    isLeftHandedAndWentToHarvard.observe(true)

    println("\n--- C ---")
    println("VE: " + VariableElimination.probability(isPresident, true))

    val algorithmCImp = Importance(10000, isPresident)
    algorithmCImp.start()
    println("Importance: " + algorithmCImp.probability(isPresident, true))
    algorithmCImp.kill()

    val algorithmCBP = BeliefPropagation(100, isPresident)
    algorithmCBP.start()
    println("BP: " + algorithmCBP.probability(isPresident, true))
    algorithmCBP.kill()
    isLeftHandedAndWentToHarvard.unobserve()
  }
}
