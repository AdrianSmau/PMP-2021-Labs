package Lab5

import com.cra.figaro.algorithm.sampling.Importance
import com.cra.figaro.language.{Apply, Constant, Element, Flip, Chain}
import com.cra.figaro.library.compound.{If, CPD, ^^}

object Ex6 {
  def tennis(
      probP1ServeWin: Double,
      probP1Winner: Double,
      probP1Nets: Double,
      probP1Out: Double,
      probP2ServeWin: Double,
      probP2Winner: Double,
      probP2Nets: Double,
      probP2Out: Double,
      refOutBias: Double, // probability for the referee to consider an Out ball as Valid
      refInBias: Double // probability for the referee to consider a Valid ball as Out
  ): Element[Boolean] = {
    def rally(firstShot: Boolean, player1: Boolean): Element[Boolean] = {

      // We compute the probability for the current player to hit nets
      val hitsNets = if (player1) Flip(probP1Nets) else Flip(probP2Nets)

      val pWinner =
        if (firstShot && player1) probP1ServeWin
        else if (firstShot && !player1) probP2ServeWin
        else if (player1) probP1Winner
        else probP2Winner

      // We compute the probability for the current player to send the ball out
      val pErrorOut = if (player1) probP1Out else probP2Out
      val winner = Flip(pWinner)
      // We compute the initial error probability, before applying the referee bias
      val initialError = Flip(pErrorOut)

      // REFEREE IMPLEMENTATION - we only need to negate (if the referee is biased in certain scenarios) the initial error in order to successfully apply the referee factor to out code
      val error = If(
        initialError,
        If(
          Flip(refOutBias),
          !initialError,
          initialError
        ), // If the player actually made a mistake => If the referee is biased, the mistake is negated, else the mistake remains as it was
        If(
          Flip(refInBias),
          !initialError,
          initialError
        ) // If the player doesn't make a mistake => If the referee is biased, the good hit is marked as a mistake, else the good hit remains as it is
      )

      If(
        winner,
        Constant(player1), // If current player wins, this is straightforward
        If(
          error, // If player1 doesn't win outright, we check if the current player made a mistake by sending it out
          Constant(!player1), // If so, the other player wins
          If(
            hitsNets, // If no errors were made by the current player
            if (firstShot)
              rally(
                false,
                player1
              ) // If the current player hits the net and this is the first net, we let him serve again
            else
              Constant(
                !player1
              ), // If this is the second net, the rally goes to the other player
            Constant(
              !player1
            ) // If the winner isn't the current player, and no errors (either out nor nets) take place, the point goes to the other player
          )
        )
      )
    }

    def game(
        p1Serves: Boolean,
        p1Points: Element[Int],
        p2Points: Element[Int]
    ): Element[Boolean] = {
      val p1WinsPoint = rally(true, p1Serves)

      val newP1Points = Apply(
        p1WinsPoint,
        p1Points,
        (wins: Boolean, points: Int) => if (wins) points + 1 else points
      )

      val newP2Points = Apply(
        p1WinsPoint,
        p2Points,
        (wins: Boolean, points: Int) => if (wins) points else points + 1
      )

      val p1WinsGame = Apply(
        newP1Points,
        newP2Points,
        (p1: Int, p2: Int) => p1 >= 4 && p1 - p2 >= 2
      )

      val p2WinsGame = Apply(
        newP2Points,
        newP1Points,
        (p2: Int, p1: Int) => p2 >= 4 && p2 - p1 >= 2
      )

      val gameOver = p1WinsGame || p2WinsGame

      If(gameOver, p1WinsGame, game(p1Serves, newP1Points, newP2Points))
    }

    def play(
        p1Serves: Boolean,
        p1Sets: Element[Int],
        p2Sets: Element[Int],
        p1Games: Element[Int],
        p2Games: Element[Int]
    ): Element[Boolean] = {
      val p1WinsGame = game(p1Serves, Constant(0), Constant(0))
      val newP1Games = Apply(
        p1WinsGame,
        p1Games,
        p2Games,
        (wins: Boolean, p1: Int, p2: Int) =>
          if (wins) {
            if (p1 >= 5) 0 else p1 + 1
          } else {
            if (p2 >= 5) 0 else p1
          }
      )

      val newP2Games = Apply(
        p1WinsGame,
        p1Games,
        p2Games,
        (wins: Boolean, p1: Int, p2: Int) =>
          if (wins) {
            if (p1 >= 5) 0 else p2
          } else {
            if (p2 >= 5) 0 else p2 + 1
          }
      )

      val newP1Sets = Apply(
        p1WinsGame,
        p1Games,
        p1Sets,
        (wins: Boolean, games: Int, sets: Int) =>
          if (wins && games == 5)
            sets + 1
          else
            sets
      )

      val newP2Sets = Apply(
        p1WinsGame,
        p2Games,
        p2Sets,
        (wins: Boolean, games: Int, sets: Int) =>
          if (!wins && games == 5)
            sets + 1
          else
            sets
      )

      val matchOver =
        Apply(newP1Sets, newP2Sets, (p1: Int, p2: Int) => p1 >= 2 || p2 >= 2)

      If(
        matchOver,
        Apply(newP1Sets, (sets: Int) => sets >= 2),
        play(!p1Serves, newP1Sets, newP2Sets, newP1Games, newP2Games)
      )
    }

    play(true, Constant(0), Constant(0), Constant(0), Constant(0))
  }

  def main(args: Array[String]) {
    //SCENARIO 1
    val tennis_match1 = tennis(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.1, 0.1)
    // Since all the probabilities are equally 0.5, this should be ~0.5 each time => the players have the same 'stats', and the referee bias is the same for both players, so this shouldn't be a factor
    val alg1 = Importance(300, tennis_match1)
    alg1.start()
    alg1.stop()
    println(
      "[SCENARIO 1] Expected gain:" + alg1.probability(tennis_match1, true)
    )

    //SCENARIO 2
    val tennis_match2 =
      tennis(0.6, 0.45, 0.3, 0.3, 0.45, 0.7, 0.35, 0.25, 0.05, 0.05)
    // Here, the first player is better at serving the ball, but the second player is better when it comes to responding to serves. The first player is slighty less likely to hit nets, but the second player is slightly less likely to out the ball
    // The referee biases are 5% for both in and out balls
    val alg2 = Importance(300, tennis_match2)
    alg2.start()
    alg2.stop()
    println(
      "[SCENARIO 2] Expected gain:" + alg2.probability(tennis_match2, true)
    )

    //SCENARIO 3
    val tennis_match3 =
      tennis(0.55, 0.4, 0.35, 0.25, 0.65, 0.5, 0.7, 0.6, 0.15, 0.15)
    // Here, the first player is not that good at serving or responding, however he makes less mistakes than the second player
    // The referees are bad, though
    val alg3 = Importance(300, tennis_match3)
    alg3.start()
    alg3.stop()
    println(
      "[SCENARIO 3] Expected gain:" + alg3.probability(tennis_match3, true)
    )

	//SCENARIO 4
    val tennis_match4 =
      tennis(0.75, 0.68, 0.25, 0.22, 0.77, 0.8, 0.22, 0.2, 0.02, 0.02)
    // The match is between two players of similar skill stats, with minimal referee bias
    val alg4 = Importance(300, tennis_match4)
    alg4.start()
    alg4.stop()
    println(
      "[SCENARIO 4] Expected gain:" + alg4.probability(tennis_match4, true)
    )
  }
}
