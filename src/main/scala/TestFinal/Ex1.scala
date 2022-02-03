package TestFinal

import com.cra.figaro.library.atomic.discrete.{FromRange}
import com.cra.figaro.language.{
  Chain,
  Universe,
  Element,
  Constant,
  Flip,
  Select,
  Apply
}
import com.cra.figaro.library.compound.{RichCPD, CPD, OneOf, If, ^^, *}
import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.algorithm.filtering._
import com.cra.figaro.algorithm.factored.beliefpropagation.BeliefPropagation
import com.cra.figaro.algorithm.sampling._

object Ex1 {
  class Autor {
    val popular = Select(0.1 -> true, 0.5 -> false)
  }
  class Album(val autor: Autor) {
    val creatorPopularity = autor.popular
    val calitate = Select(0.27 -> "mica", 0.6 -> "medie", 0.13 -> "mare")
  }
  class Nominalizare(val album: Album) {
    def getProb(
        creatorPopular: Boolean,
        quality: String
    ): Element[Double] = {
      if (quality == "mica") {
        if (creatorPopular == true)
          Constant(0.014)
        else
          Constant(0.003)
      }
      if (quality == "medie") {
        if (creatorPopular == true)
          Constant(0.043)
        else
          Constant(0.016)
      }
      if (creatorPopular == true)
        Constant(0.18)
      Constant(0.047)
    }

    val prob = Apply(
        album.creatorPopularity,
        album.calitate,
        (popular: Boolean, qual: String) => getProb(popular, qual)
      )

    val nominalizat = (
      Apply(
        album.creatorPopularity,
        album.calitate,
        (popular: Boolean, qual: String) => getProb(popular, qual)
      ),
      (prob: Double) => Flip(prob)
    )
  }

  def main(args: Array[String]) {
    val autors: Array[Autor] = Array.fill(5)(new Autor())

    val rand = new scala.util.Random

    val albums: Array[Album] =
      Array.fill(10)(new Album(autors(rand.nextInt(5))))

    val nominalizari: Array[Nominalizare] = Array.tabulate(10)((iteration: Int) => new Nominalizare(albums(iteration)))

    val algoritm = MetropolisHastings(100000, ProposalScheme.default, nominalizari(0).prob)
    algoritm.start()
    //println("Probabilitatea medie ca primul album sa fie nominalizat este: " + algoritm.mean(nominalizari(0).prob))

  }
}
