package net.fosd.vgit

import org.scalameter.DSL.measure
import org.scalameter.DSL.performance
import org.scalameter.Gen
import org.scalameter.PerformanceTest
import org.scalameter._

class PerformanceTest extends PerformanceTest.Quickbenchmark {

    val text = """Et licet quocumque oculos flexeris feminas adfatim multas spectare cirratas, quibus, si nupsissent, per aetatem ter iam
                 |nixus poterat suppetere liberorum, ad usque taedium pedibus pavimenta tergentes iactari volucriter gyris, dum exprimunt innumera
                 |simulacra, quae finxere fabulae theatrales.%0A%0AHaec subinde Constantius audiens et quaedam referente Thalassio doctus, quem eum odisse
                 |iam conpererat lege communi, scribens ad Caesarem blandius adiumenta paulatim illi subtraxit, sollicitari se simulans ne, uti est militare
                 |otium fere tumultuosum, in eius perniciem conspiraret, solisque scholis iussit esse contentum palatinis et protectorum cum Scutariis et
                 |Gentilibus, et mandabat Domitiano, ex comite largitionum, praefecto ut cum in Syriam venerit, Gallum, quem crebro acciverat, ad Italiam
                 |properare blande hortaretur et verecunde.%0A%0ASed si ille hac tam eximia fortuna propter utilitatem rei publicae frui non properat, ut
                 |omnia illa conficiat, quid ego, senator, facere debeo, quem, etiamsi ille aliud vellet, rei publicae consulere oporteret?%0AEt licet
                 |quocumque oculos flexeris feminas adfatim multas spectare cirratas, quibus, si nupsissent, per aetatem ter iam nixus poterat suppetere
                 |liberorum, ad usque taedium pedibus pavimenta tergentes iactari volucriter gyris, dum exprimunt innumera simulacra, quae finxere fabulae
                 |theatrales.%0A%0AHaec subinde Constantius audiens et quaedam referente Thalassio doctus, quem eum odisse iam conpererat lege communi,
                 |scribens ad Caesarem blandius adiumenta paulatim illi subtraxit, sollicitari se simulans ne, uti est militare otium fere tumultuosum, in
                 |eius perniciem conspiraret, solisque scholis iussit esse contentum palatinis et protectorum cum Scutariis et Gentilibus, et mandabat
                 |Domitiano, ex comite largitionum, praefecto ut cum in Syriam venerit, Gallum, quem crebro acciverat, ad Italiam properare blande
                 |hortaretur et verecunde.%0A%0ASed si ille hac tam eximia fortuna propter utilitatem rei publicae frui non properat, ut omnia illa
                 |conficiat, quid ego, senator, facere debeo, quem, etiamsi ille aliud vellet, rei publicae consulere oporteret?%0AEt licet quocumque oculos
                 |flexeris feminas adfatim multas spectare cirratas, quibus, si nupsissent, per aetatem ter iam nixus poterat suppetere liberorum, ad usque
                 |taedium pedibus pavimenta tergentes iactari volucriter gyris, dum exprimunt innumera simulacra, quae finxere fabulae theatrales.%0A%0AHaec
                 |subinde Constantius audiens et quaedam referente Thalassio doctus, quem eum odisse iam conpererat lege communi, scribens ad Caesarem
                 |blandius adiumenta paulatim illi subtraxit, sollicitari se simulans ne, uti est militare otium fere tumultuosum, in eius perniciem
                 |conspiraret, solisque scholis iussit esse contentum palatinis et protectorum cum Scutariis et Gentilibus, et mandabat Domitiano, ex comite
                 |largitionum, praefecto ut cum in Syriam venerit, Gallum, quem crebro acciverat, ad Italiam properare blande hortaretur et
                 |verecunde.%0A%0ASed si ille hac tam eximia fortuna propter utilitatem rei publicae frui non properat, ut omnia illa conficiat, quid ego,
                 |senator, facere debeo, quem, etiamsi ille aliud vellet, rei publicae consulere oporteret?%0A""".stripMargin.replace("\n", "")

    val textGen = Gen.single("text")(text)

    val reGen =
        for(re <- Gen.single("re")("([-A-Za-z0-9_.!~*'();/?:@&=+$,# ]|%[A-Fa-f0-9]{2})+".re))
            yield {
                // force evaluation to make it compile
                re.isMatchedBy("")
                re
            }

    val inputs = Gen.tupled(textGen, reGen)

    performance of "New regular expression" in {
        measure method "findFirstIn" in {

            using(textGen) in { t =>
                val localRe = "([-A-Za-z0-9_.!~*'();/?:@&=+$,# ]|%[A-Fa-f0-9]{2})+".re

                localRe.findFirstIn(t)
            }

        }
    }

    performance of "Reused regular expression" in {
        measure method "findFirstIn" in {

            using(inputs) in { case (t, re) =>
                re.findFirstIn(t)
            }

        }
    }

}

