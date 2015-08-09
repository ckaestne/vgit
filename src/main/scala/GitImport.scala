package net.fosd.vgit

import org.eclipse.jgit.api._
import org.eclipse.jgit.diff.DiffEntry.Side._
import org.eclipse.jgit.diff._
import org.eclipse.jgit.errors.{AmbiguousObjectException, MissingObjectException}
import org.eclipse.jgit.lib._
import org.eclipse.jgit._
import org.eclipse.jgit.revwalk.{RevCommit, RevWalk}
import org.eclipse.jgit.storage.file.FileRepositoryBuilder
import java.io._
import org.eclipse.jgit.treewalk.filter.PathFilter
import org.eclipse.jgit.treewalk.{TreeWalk, CanonicalTreeParser, AbstractTreeIterator}

import scala.collection.JavaConversions._

/**
 * Created by ckaestne on 6/28/2015.
 */
object GitImport extends App {
    assert(args.length == 2)
    val _gitProject = args(0)
    val _file = args(1)

    val lineTable = genLineTable(_gitProject, _file)
    val fw = new FileWriter("out.c")
    fw.write(lineTable.toIfdef())
    fw.close()


    def genLineTable(gitProject: String, file: String): LineTable = {


        val builder = new FileRepositoryBuilder()
        val repository = builder.setGitDir(new File(gitProject))
            .readEnvironment() // scan environment GIT_* variables
            .findGitDir() // scan up the file system tree
            .build();

        val walk = new RevWalk(repository)
        val diffAlg = DiffAlgorithm.getAlgorithm(DiffAlgorithm.SupportedAlgorithm.HISTOGRAM)
        var comparator: RawTextComparator = RawTextComparator.DEFAULT
        val EMPTY = Array[Byte]()
        val reader = repository.newObjectReader
        val cs: ContentSource = ContentSource.create(reader)

        val commits = new Git(repository).log()
            .all().addPath(file)
            .call().toList.reverse


        var lineTable = new LineTable()
        var counter = 0

        for (commit <- commits) processCommit(commit)


        repository.close();





        def processCommit(commit: RevCommit): Unit = {
            println(commit.getShortMessage + " -- " + commit.getName)
            assert(commit.getParents.forall(p => lineTable.commitIds.contains(p.getName)), "parent commit not processed yet %s -> %s".format(commit.getName, commit.getParents.map(_.getName).mkString(", ")))

            if (commit.getParentCount == 0) {
                val initialFileContent = getFileContent(repository, commit, file)
                lineTable.init(commit.getName, initialFileContent)
                println("initial table: ")
                println(lineTable)
            }
            else {
                val current = prepareTreeParser(commit)
                val parent = prepareTreeParser(commit.getParents.head)
                val diff = new Git(repository).diff().setOldTree(parent).setNewTree(current).setPathFilter(PathFilter.create(file)).call()
                assert(diff.size() <= 1)
                for (entry <- diff) {
                    counter += 1
                    println("processing %d %s".format(counter, commit.getName))
                    val (diffEntry, newText) = diffText(entry)
                    lineTable.update(commit.getName, commit.getParents.head.getName, diffEntry, newText)
                    //                println("table after %s: ".format(commit.getName))
                }
                if (diff.isEmpty)
                    lineTable.update(commit.getName, commit.getParents.head.getName, new EditList(), new RawText(new Array[Byte](0)))
            }
        }


        def diffText(entry: DiffEntry): (EditList, RawText) = {
            val aRaw: Array[Byte] = open(OLD, entry)
            val bRaw: Array[Byte] = open(NEW, entry)
            val a = new RawText(aRaw)
            val b = new RawText(bRaw)
            (diffAlg.diff(comparator, a, b), b)
        }

        def open(side: DiffEntry.Side, entry: DiffEntry): Array[Byte] = {
            val id = entry.getId(side)
            assert(id.isComplete)
            open_(entry.getPath(side), id.toObjectId)
        }

        def open_(path: String, id: ObjectId): Array[Byte] = {
            val loader = cs.open(path, id)
            loader.getBytes
        }

        def prepareTreeParser(commit: RevCommit): AbstractTreeIterator = {
            // from the commit we can build the tree which allows us to construct the TreeParser

            val tree = walk.parseTree(commit.getTree().getId())

            val oldTreeParser = new CanonicalTreeParser()
            val oldReader = repository.newObjectReader()
            try {
                oldTreeParser.reset(oldReader, tree.getId())
            } finally {
                oldReader.close()
            }

            return oldTreeParser;
        }



        lineTable
    }


    def getFileContent(repository: Repository, commit: RevCommit, file: String): RawText = {
        val tree = commit.getTree()
        val treeWalk = new TreeWalk(repository)
        treeWalk.addTree(tree)
        treeWalk.setRecursive(true)
        treeWalk.setFilter(PathFilter.create(file))
        if (!treeWalk.next()) {
            return new RawText(new Array[Byte](0))
        }
        val objectId = treeWalk.getObjectId(0)
        val loader = repository.open(objectId)
        new RawText(loader.getBytes)
    }

}
