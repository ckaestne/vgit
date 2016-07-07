package net.fosd.vgit

//import org.scalameter.DSL.measure
//import org.scalameter.DSL.performance

import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.featureexpr.{FeatureExpr, FeatureExprFactory}
import edu.cmu.feature.vregex._
import gnieh.regex._
import org.scalameter.{Gen, PerformanceTest}

class ForkPerformanceTest extends PerformanceTest.Quickbenchmark {

//    FeatureExprFactory.setDefault(FeatureExprFactory.bdd)

//    val gitProject = "test/.git"
//    val file = "test2.c"

            val gitProject = "c:/users/ckaestne/linux/.git"
            val file = "kernel/fork.c"


    val fileRevisions = GitHelper.getFileRevisions(gitProject, file)

    val plainContents: List[String] = fileRevisions.map(x => x._2.getString(0, x._2.size(), false))

    val lineTable = GitImport.genLineTable(gitProject, file)

    val vcontent = lineTableToVString(lineTable)

    def lineTableToVString(lineTable: LineTable): VString = {
        def getCondition(revisionPositions: List[Int]): FeatureExpr =
            (revisionPositions zip lineTable.commitIds).
                filter(_._1 >= 0).
                map(x => FeatureExprFactory.createDefinedExternal(x._2)).
                foldRight(FeatureExprFactory.False)(_ or _)

        val lines = for (l <- lineTable.lines)
            yield ChoiceStr(getCondition(l.revisionPositions), l.text, "")
        Concatenate(lines)
    }

    val uncompressedSize = plainContents.map(_.size).reduce(_ + _)
    val compressedSize = vcontent.toVCharList.size
    assert(plainContents.size == lineTable.commitIds.size)
    println("commits: " + plainContents.size)
    println("uncompressed size: " + uncompressedSize)
    println("compressed size: " + compressedSize)
    println("linear compression rate (reference): %.3f".format(1d - 1d / plainContents.size))
    println("compression rate: %.3f".format(1d - compressedSize.toDouble / uncompressedSize.toDouble))

    val vinput = vcontent.toVCharList
    val vinputbruteforce = plainContents.map(AString(_).toVCharList)
    val inputbruteforce = plainContents

    val expressions: Gen[String] = Gen.enumeration("expression")("a", "aaaaaaaaa")
    val expressionsR = (for (e <- expressions) yield (e.vre, e.re, e.r)).cached


    //    val reGen =
    //        for (re <- Gen.single("re")("([-A-Za-z0-9_.!~*'();/?:@&=+$,# ]|%[A-Fa-f0-9]{2})+".vre))
    //            yield {
    //                // force evaluation to make it compile
    //                re.isMatchedBy("")
    //                re
    //            }
    //
    //    val inputs = Gen.tupled(textGen, reGen)

    performance of "search for \"a\"" in {
//        measure method "vregex - variational" in {
//            using(expressionsR) in {
//                _._1 isMatchedBy vinput
//            }
//        }
        measure method "vregex - brute force" in {
            using(expressionsR) in {
                vinputbruteforce map _._1.isMatchedBy
            }
        }
        measure method "regex - brute force" in {
            using(expressionsR) in {
                inputbruteforce map _._2.isMatchedBy
            }
        }
        measure method "java.regex - brute force" in {
            using(expressionsR) in {
                inputbruteforce map _._3.findFirstIn
            }
        }
    }


    //
    //    performance of "Reused regular expression" in {
    //        measure method "findFirstIn" in {
    //
    //            using(inputs) in { case (t, re) =>
    //                re.isMatchedBy(t)
    //            }
    //
    //        }
    //    }

}

