package Lab4

import com.cra.figaro.library.atomic.discrete
import com.cra.figaro.language.Chain
import com.cra.figaro.library.compound.{RichCPD, OneOf, *}
import com.cra.figaro.language.{Flip, Constant, Apply}
import com.cra.figaro.algorithm.factored.VariableElimination

object Ex5 {
  def main(args: Array[String]) {
    // To keep the code simple, I just make the cards an integer
    val cards = List(5, 4, 3, 2, 1)
    // The discrete uniform distribution chooses uniformly from a fixed set of possibilities
    val player1Card = discrete.Uniform(cards: _*)
    val player2Card = Chain(
      player1Card,
      (card: Int) =>
        discrete.Uniform(
          cards.filter(_ != card): _*
        ) // Player 2 can get any card except the first player’s card
    )

    val player1Bet1 = RichCPD(
      player1Card,
      // Player 1 is more likely to bet with a higher card,
      // but will sometimes bet with a lower card to bluff
      OneOf(5, 4, 3) -> Flip(0.9),
      * -> Flip(0.4)
    )

    val player2Bet = RichCPD(
      player2Card,
      player1Bet1,
      (OneOf(5, 4), *) -> Flip(0.9),
      (*, OneOf(false)) -> Flip(0.5),
      (*, *) -> Flip(0.1)
    )

    val player1Bet2 =
      Apply(
        player1Card,
        player1Bet1,
        player2Bet,
        (card: Int, bet11: Boolean, bet2: Boolean) =>
          // Player 1’s second bet is only relevant if she passed the
          // first time and player 2 bet
          !bet11 && bet2 && (card == 5 || card == 4)
      )
    // This element represents the gain to player 1 from the game. I have
    // made it an Element[Double] so I can query its mean.
    val player1Gain = Apply(
      player1Card,
      player2Card,
      player1Bet1,
      player2Bet,
      player1Bet2,
      (card1: Int, card2: Int, bet11: Boolean, bet2: Boolean, bet12: Boolean) =>
        if (!bet11 && !bet2) 0.0
        else if (bet11 && !bet2) 1.0
        else if (!bet11 && bet2 && !bet12) -1.0
        else if (card1 > card2) 2.0
        else -2.0
    )
    // For solving exercise 5, we know that computing the probability of a certain world, we need to multiply the probability of each node of the generated decision tree
    // First, we generate random scenarios as we consider them, then we compute each node's probability, and finally we multiply them in order to get the probability of that particular
    // world
    println(
      "[SCENARIO 1] Player1 draws a 5, player2 draws a 4, player1 elects to pass, player2 elects to bet, finally player1 elects to bet as well"
    )
    var probability11 = VariableElimination.probability(player1Card, 5)
    println("Player1 draws a 5: " + probability11)
    player1Card.observe(5)
    var probability12 = VariableElimination.probability(player2Card, 4)
    println("Player2 draws a 4: " + probability12)
    player2Card.observe(4)
    var probability13 = VariableElimination.probability(player1Bet1, false)
    println("Player1 elects to pass first: " + probability13)
    player1Bet1.observe(false)
    var probability14 = VariableElimination.probability(player2Bet, true)
    println("Player2 elects to bet: " + probability14)
    player2Bet.observe(true)
    var probability15 = VariableElimination.probability(player1Bet2, true)
    println("Player1 then elects to bet: " + probability15)
    player1Card.unobserve()
    player2Card.unobserve()
    player1Bet1.unobserve()
    player2Bet.unobserve()
    println(
      "The total probability of this particular world is: " + (probability11 * probability12 * probability13 * probability14 * probability15)
    )

    val totalS1 = Apply(
      player1Card,
      player2Card,
      player1Bet1,
      player2Bet,
      player1Bet2,
      (i1: Int, i2: Int, i3: Boolean, i4: Boolean, i5: Boolean) =>
        i1 == 5 && i2 == 4 && i3 == false && i4 == true && i5 == true
    )
    println(
      "\n[SOLUTION SCENARIO 1] Using Apply instead of observe: " + VariableElimination
        .probability(totalS1, true)
    )

    println(
      "\n[SCENARIO 2] Player1 draws a 2, player2 draws a 3, player1 elects to bet, player2 elects to pass"
    )
    var probability21 = VariableElimination.probability(player1Card, 2)
    println("Player1 draws a 2: " + probability21)
    player1Card.observe(2)
    var probability22 = VariableElimination.probability(player2Card, 3)
    println("Player2 draws a 3: " + probability22)
    player2Card.observe(3)
    var probability23 = VariableElimination.probability(player1Bet1, true)
    println("Player1 elects to bet: " + probability23)
    player1Bet1.observe(true)
    var probability24 = VariableElimination.probability(player2Bet, false)
    println("Player2 elects to fold: " + probability24)
    player1Card.unobserve()
    player2Card.unobserve()
    player1Bet1.unobserve()
    println(
      "The total probability of this particular world is: " + (probability21 * probability22 * probability23 * probability24)
    )

    val totalS2 = Apply(
      player1Card,
      player2Card,
      player1Bet1,
      player2Bet,
      (i1: Int, i2: Int, i3: Boolean, i4: Boolean) =>
        i1 == 2 && i2 == 3 && i3 == true && i4 == false
    )
    println(
      "\n[SOLUTION SCENARIO 2] Using Apply instead of observe: " + VariableElimination
        .probability(totalS2, true)
    )

    println(
      "\n[SCENARIO 3] Player1 draws a 2, player2 draws a 5, player1 elects to bet, player2 elects to bet, finally player1 elects to pass"
    )
    var probability31 = VariableElimination.probability(player1Card, 2)
    println("Player1 draws a 2: " + probability31)
    player1Card.observe(2)
    var probability32 = VariableElimination.probability(player2Card, 5)
    println("Player2 draws a 5: " + probability32)
    player2Card.observe(5)
    var probability33 = VariableElimination.probability(player1Bet1, true)
    println("Player1 elects to bet: " + probability33)
    player1Bet1.observe(true)
    var probability34 = VariableElimination.probability(player2Bet, true)
    println("Player2 elects to bet: " + probability34)
    player2Bet.observe(true)
    var probability35 = VariableElimination.probability(player1Bet2, false)
    println("Player1 then elects to fold: " + probability35)
    player1Card.unobserve()
    player2Card.unobserve()
    player1Bet1.unobserve()
    player2Bet.unobserve()
    println(
      "The total probability of this particular world is: " + (probability31 * probability32 * probability33 * probability34 * probability35)
    )

    val totalS3 = Apply(
      player1Card,
      player2Card,
      player1Bet1,
      player2Bet,
      player1Bet2,
      (i1: Int, i2: Int, i3: Boolean, i4: Boolean, i5: Boolean) =>
        i1 == 2 && i2 == 5 && i3 == true && i4 == true && i5 == false
    )
    println(
      "\n[SOLUTION SCENARIO 3] Using Apply instead of observe: " + VariableElimination
        .probability(totalS3, true)
    )
  }
}
