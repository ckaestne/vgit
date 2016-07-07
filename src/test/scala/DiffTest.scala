package net.fosd.vgit

import java.io.File

import org.eclipse.jgit.api._
import org.eclipse.jgit.diff.RawText
import org.eclipse.jgit.revwalk.RevCommit
import org.eclipse.jgit.storage.file.FileRepositoryBuilder
import org.scalatest.FunSuite

import scala.collection.JavaConversions._


class DiffTest extends FunSuite {


    test("test/test.c should be equivalent to all inputs") {
        val gitProject = "test/.git"
        val file = "test.c"
        check(gitProject, file)
    }

    test("test/test2.c should be equivalent to all inputs") {
        val gitProject = "test/.git"
        val file = "test2.c"
        check(gitProject, file)
    }

    //    test("linux/kernel/fork.c should be equivalent to all inputs") {
    //        val gitProject = "c:/users/ckaestne/linux/.git"
    //        val file = "kernel/fork.c"
    //        check(gitProject, file)
    //    }

    def check(gitProject: String, file: String) {
        val lineTable = GitImport.genLineTable(gitProject, file)



        GitHelper.getFileRevisions(gitProject, file).map(
            x => compareCommit(lineTable, x._1, x._2))

    }

    def compareCommit(lineTable: LineTable, commitId: String, fileContent: RawText) {
        if (!(lineTable.commitIds contains commitId)) return;

        println("== checking commit %s".format(commitId))
        val gitLines = for (i <- 0 until fileContent.size();
                            if fileContent.getString(i).trim.nonEmpty)
            yield fileContent.getString(i)

        val preprocessedLines = lineTable.getCommit(commitId).filterNot(_.trim.isEmpty)

        lazy val errormsg = "unexpected output for %s -- expected:\n%s\nfound:\n%s\n".format(commitId, gitLines.mkString("\n"), preprocessedLines.mkString("\n"))

        assert(gitLines.size == preprocessedLines.size, errormsg)
        for ((a, b) <- gitLines.zip(preprocessedLines))
            assert(a.trim == b.trim, errormsg)
    }

}

object GitHelper {
    def getFileRevisions(gitProject: String, file: String): List[(String, RawText)] = {
        val builder = new FileRepositoryBuilder()
        val repository = builder.setGitDir(new File(gitProject))
            .readEnvironment() // scan environment GIT_* variables
            .findGitDir() // scan up the file system tree
            .build();

        val commits = new Git(repository).log()
            .all().addPath(file)
            .call().toList.reverse

        for (commit <- commits)
            yield (commit.getName, GitImport.getFileContent(repository, commit, file))
    }
}