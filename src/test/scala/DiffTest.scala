package net.fosd.vgit

import java.io.File

import org.eclipse.jgit.api._
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

    test("linux/kernel/fork.c should be equivalent to all inputs") {
        val gitProject = "c:/users/ckaestne/linux/.git"
        val file = "kernel/fork.c"
        check(gitProject, file)
    }

    def check(gitProject: String, file: String) {
        val lineTable = GitImport.genLineTable(gitProject, file)


        val builder = new FileRepositoryBuilder()
        val repository = builder.setGitDir(new File(gitProject))
            .readEnvironment() // scan environment GIT_* variables
            .findGitDir() // scan up the file system tree
            .build();

        val commits = new Git(repository).log()
            .all().addPath(file)
            .call().toList.reverse


        def compareCommit(commit: RevCommit) {
            if (!(lineTable.commitIds contains commit.getName)) return;

            println("== checking commit %s".format(commit.getName))
            val fileContent = GitImport.getFileContent(repository, commit, file)
            val gitLines = for (i <- 0 until fileContent.size();
                                if fileContent.getString(i).trim.nonEmpty)
                yield fileContent.getString(i)

            val preprocessedLines = lineTable.getCommit(commit.getName).filterNot(_.trim.isEmpty)

            lazy val errormsg = "unexpected output for %s -- expected:\n%s\nfound:\n%s\n".format(commit.getName, gitLines.mkString("\n"), preprocessedLines.mkString("\n"))

            assert(gitLines.size == preprocessedLines.size, errormsg)
            for ((a, b) <- gitLines.zip(preprocessedLines))
                assert(a.trim == b.trim, errormsg)
        }

        for (commit <- commits) compareCommit(commit)
    }


}