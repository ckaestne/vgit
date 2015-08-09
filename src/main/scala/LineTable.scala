package net.fosd.vgit

import org.eclipse.jgit.diff.{RawText, Edit, EditList}
import scala.collection.JavaConversions._


/**
 * line table indicates which revision contains which line in which place
 */
class LineTable {


    def init(id: GitCommitId, initialContent: RawText): Unit = {
        commitIds ::= id
        lines = List()
        for (i <- 0 until initialContent.size())
            lines ::= Line("initial-L" + i, initialContent.getString(i), List(i))
    }

    def update(id: GitCommitId, parentid: GitCommitId, edits: EditList, newText: RawText) = {
        val parentIdx = commitIds.indexOf(parentid)
        commitIds ::= id

        lines = for (l <- lines) yield {
            Line(l.id, l.text, l.revisionPositions(parentIdx) :: l.revisionPositions)
        }
        for (edit: Edit <- edits.reverse) {
            var newLines = List[Line]()
            var inserted = false
            for (lidx <- 0 until lines.size) {
                val l = lines(lidx)
                if (l.revisionPositions.head < edit.getBeginA)
                    newLines ::= l
                else if (l.revisionPositions.head >= edit.getEndA)
                    newLines ::= l.incHeadPos(edit.getLengthB - edit.getLengthA)
                else
                    newLines ::= l.setHeadPos(-1)

                if (l.revisionPositions.head == edit.getEndA) {
                    for (newline <- (0 until edit.getLengthB).reverse)
                        newLines ::= Line(id + "-L" + (edit.getBeginB + newline), newText.getString(edit.getBeginB + newline), List.fill(commitIds.size)(-1)).setHeadPos(edit.getBeginA + newline)
                    inserted = true
                }
            }
            if (!inserted)
                for (newline <- 0 until edit.getLengthB)
                    newLines = newLines :+ Line(id + "-L" + (edit.getBeginB + newline), newText.getString(edit.getBeginB + newline), List.fill(commitIds.size)(-1)).setHeadPos(edit.getBeginA + newline)

            lines = newLines.reverse
            //            }
        }
    }

    override def toString() = {
        (for (l <- lines) yield
        l.id + ": " + l.revisionPositions.mkString(" ")).mkString("\n")
    }

    def getCondition(l: Line): String =
        (l.revisionPositions zip commitIds).filter(_._1 >= 0).map("C" + _._2).mkString("defined(", ") || defined(", ")")

    def toIfdef(): String = {
        val out = new StringBuffer()
        var lastLineCondition: String = ""

        for (l <- lines.reverse) {
            val condition: String = getCondition(l)

            if (condition != lastLineCondition) {
                if (lastLineCondition != "")
                    out.append("#endif\n")
                if (condition != "")
                    out.append("#if ").append(condition).append("\n")
            }
            lastLineCondition = condition

            out.append(l.text).append("\n")
        }
        if (lastLineCondition != "")
            out.append("#endif\n")
        out.toString
    }

    /**
     * return the content of one commit extracted from the
     * condensed representation
     */
    def getCommit(commitId: GitCommitId):List[String] = {
        val idx = commitIds.indexOf(commitId)
        lines.filter(_.revisionPositions(idx)>=0).map(_.text).reverse
    }


    type CommitNr = Int
    type GitCommitId = String
    type LineNr = Int

    var commitIds: List[GitCommitId] = List()
    var lines: List[Line] = List()

    case class Line(id: String, text: String, revisionPositions: List[LineNr]) {
        def incHeadPos(by: Int) = {
            var h = revisionPositions.head
            if (h != -1)
                h += by
            Line(id, text, h :: revisionPositions.tail)
        }

        def setHeadPos(to: Int) = Line(id, text, to :: revisionPositions.tail)
    }



}