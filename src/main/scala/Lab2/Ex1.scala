package Lab2

import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.language._
import com.cra.figaro.library.compound.CPD
import com.cra.figaro.algorithm.sampling._

object Ex1 {
    def main(args: Array[String]) {
        val reducere = Flip(0.15) // Exista o sansa de 15% ca produsul sa fie redus
        val nevoie = Flip(0.2) // Exista o sansa de 20% ca respectivul cumparator sa aiba nevoie de produs
        val result = CPD(reducere,nevoie,
            (true, true) -> Flip(1), // produsul este la reduceresi ai nevoie de el -> cumperi 100%
            (true, false) -> Flip(0.5), // produsul este la reducere dar nu ai nevoie de el -> cumperi 50%
            (false, true) -> Flip(0.8), // fara reducere, ai nevoie -> 80%
            (false, false) -> Flip(0.2) // fara reducere, nu ai nevoie -> 20%
        )

        result.observe(true) // Observam rezultatele in care cumparatorul a decis sa cumpere produsul
        val algorithm = VariableElimination(reducere, nevoie)
        algorithm.start()
        
        println(algorithm.probability(nevoie,true)) // Din cele observate mai sus, ne focusam pe cazurile in care cumparatorul a avut nevoie de produs
    }
}