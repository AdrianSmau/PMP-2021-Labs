package Lab4

import com.cra.figaro.library.atomic.discrete
import com.cra.figaro.language.Chain
import com.cra.figaro.library.compound.{RichCPD, OneOf, NoneOf, ^^, *}
import com.cra.figaro.language.{Flip, Constant, Apply}
import com.cra.figaro.algorithm.factored.VariableElimination

object Ex4TwoCards {
  def main(args: Array[String]) {
    // To keep the code simple, I just make the cards an integer
    val cards = List(5, 4, 3, 2, 1)
    // The discrete uniform distribution chooses uniformly from a fixed
    // set of possibilities
    val player1Card1 = discrete.Uniform(cards: _*)
    val player2Card1 = Chain(
      player1Card1,
      (card: Int) =>
        // Player 2 can get any card except the first player’s card
        discrete.Uniform(cards.filter(_ != card): _*)
    )

    val player1Card2 = Chain(
      player1Card1,
      player2Card1,
      (card1: Int, card2: Int) =>
        // Player 1 can get any card except the first player’s card 1 and second player's card 1
        discrete.Uniform(
          cards.filter(elem => (elem != card1 && elem != card2)): _*
        )
    )

    val player2Card2 = Chain(
      ^^(player1Card1, player2Card1),
      player1Card2,
      (touple: (Int, Int), card3: Int) =>
        // Player 1 can get any card except the first player’s card 1, second player's card 1 and first player's card 2
        discrete.Uniform(
          cards.filter(elem =>
            (elem != touple._1 && elem != touple._2 && elem != card3)
          ): _*
        )
    )

    val player1Bet1 = RichCPD(
      player1Card1,
      player1Card2,
      // Player 1 is more likely to bet with a higher cards,
      // but will sometimes bet with a lower card to bluff
      (OneOf(5, 4, 3), OneOf(5, 4, 3)) -> Flip(0.9),
      (NoneOf(2, 1), NoneOf(2, 1)) -> Flip(0.6),
      (*, *) -> Flip(0.4)
    )

    val player2Bet = RichCPD(
      player2Card1,
      player2Card2,
      player1Bet1,
      (OneOf(5, 4), OneOf(5, 4, 3), *) -> Flip(0.9),
      (NoneOf(2, 1), NoneOf(2, 1), OneOf(false)) -> Flip(0.6),
      (*, *, *) -> Flip(0.1)
    )

    val player1Bet2 =
      Apply(
        player1Card1,
        player2Card2,
        player1Bet1,
        player2Bet,
        (card1: Int, card2: Int, bet11: Boolean, bet2: Boolean) =>
          // Player 1’s second bet is only relevant if she passed the
          // first time and player 2 bet
          !bet11 && bet2 && (card1 + card2 >= 7) // Player 1 thinks he has call equity only if the sum of his cards is equal or greater than 7 (with 3 and 4 for example, since he blocks any other combo that would beat him)
      )

    // This element represents the gain to player 1 from the game. I have
    // made it an Element[Double] so I can query its mean.
    val player1Gain = Apply(
      ^^(^^(^^(player1Card1, player1Card2), player2Card1), player2Card2),
      player1Bet1,
      player2Bet,
      player1Bet2,
      (
          touple: (((Int, Int), Int), Int),
          bet11: Boolean,
          bet2: Boolean,
          bet12: Boolean
      ) =>
        if (!bet11 && !bet2) 0.0
        else if (bet11 && !bet2) 2.0
        else if (!bet11 && bet2 && !bet12) -2.0
        else if (touple._1._1._1 + touple._1._1._2 > touple._1._2 + touple._2) 4.0
        else -4.0
    ) // The stakes are now doubled since every player gets dealt two cards instead of one
  }
}
