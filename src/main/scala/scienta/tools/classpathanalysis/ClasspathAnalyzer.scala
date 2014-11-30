package scienta.tools.classpathanalysis

import java.io.File

import org.slf4j.Logger

class ClasspathAnalysis(pathEntries: Iterable[File]) {

  private def invertAndMap[V, K](m: Iterable[(V, K)]): Map[K, Iterable[V]] = m groupBy (_._2) mapValues (_ map (_._1))

  private val startTime = System.currentTimeMillis()

  private val fileClassesTuples: List[(FileName, Iterable[PathEntry])] = {
    def toEntry(resource: String) =
      if (resource endsWith ".class") ClassEntry(resource)
      else if (resource endsWith ".properties") PropertiesEntry(resource)
      else FileEntry(resource)

    (pathEntries map PathEntrySource map { source =>
      (FileName(source.file), source.visibleResources() map toEntry)
    }).toList
  }
  val ioTime = System.currentTimeMillis() - startTime

  val fileEntries: Map[FileName, Iterable[PathEntry]] = Map(fileClassesTuples: _*)

  val fileEntryPairs: Iterable[(FileName, PathEntry)] =
    fileClassesTuples flatMap { case (file, names) => names map ((file, _))}

  val multipleSourcePairs: Map[PathEntry, Iterable[FileName]] = invertAndMap(fileEntryPairs) filter (_._2.size > 1)

  private val irrelevantFiles = Set("META-INF/MANIFEST.MF", "META-INF/INDEX.LIST", "LICENSE.txt", "license.txt")

  private def relevantClass(entry: PathEntry) = entry.isInstanceOf[ClassEntry] && !(entry.name contains "$")

  private def relevantFile (entry: PathEntry) = entry.isInstanceOf[FileEntry] && !(irrelevantFiles contains entry.name)

  private def relevantProperties (entry: PathEntry) = entry.isInstanceOf[PropertiesEntry]

  val multipleSourceClasses: Map[ClassEntry, Iterable[FileName]] =
    (multipleSourcePairs filterKeys relevantClass).asInstanceOf[Map[ClassEntry, Iterable[FileName]]]

  val multipleSourceProperties: Map[PropertiesEntry, Iterable[FileName]] =
    (multipleSourcePairs filterKeys relevantProperties).asInstanceOf[Map[PropertiesEntry, Iterable[FileName]]]

  val multipleSourceFiles: Map[FileEntry, Iterable[FileName]] =
    (multipleSourcePairs filterKeys relevantFile).asInstanceOf[Map[FileEntry, Iterable[FileName]]]

  val classConflicts: Map[Iterable[FileName], Iterable[ClassEntry]] = invertAndMap(multipleSourceClasses)

  val propertiesConflicts: Map[Iterable[FileName], Iterable[PropertiesEntry]] = invertAndMap(multipleSourceProperties)

  val fileConflicts: Map[Iterable[FileName], Iterable[FileEntry]] = invertAndMap(multipleSourceFiles)

  def rank[E <: PathEntry](conflicts: Map[Iterable[FileName], Iterable[E]]) = conflicts.toList.sortBy(_._2.size).reverse

  val classConflictsRanked = rank(classConflicts)

  val propertiesConflictsRanked = rank(propertiesConflicts)

  val fileConflictsRanked = rank(fileConflicts)

  val time = System.currentTimeMillis() - startTime

  def filesVisibleInLoser(winner: FileName, loser: FileName)(selector: PathEntry => Boolean = entry => true): Option[Iterable[PathEntry]] =
    (fileEntries get winner map(_ filter selector) , fileEntries get loser map (_ filter selector)) match {
      case (Some(winnerResources), Some(loserResources)) =>
        def isIn(entries: Iterable[PathEntry])(entry: PathEntry) = winnerResources exists (entry == _)
        Some(loserResources filterNot isIn(winnerResources))
      case _ => None
    }
}

object ClasspathAnalysis {

  type Printer = (String) => Unit

  def logInfo(logger: Logger) = if (logger.isInfoEnabled) apply(logger info)

  def logDebug(logger: Logger) = if (logger.isDebugEnabled) apply(logger debug)

  def main(args: Array[String]): Unit = ClasspathAnalysis(println)

  def apply(implicit p: Printer): Unit = {
    implicit val analysis = new ClasspathAnalysis(classpath split pathSep map (new File(_)))
    val start = System.currentTimeMillis()
    printClassPathIssues
    p("")
    printPropertiesIssues(analysis, p)
    p("")
    printResourcesIssues(analysis, p)
    p("")
    p(s"${analysis.fileEntryPairs.size} resources analyzed in ${analysis.time}ms, ${analysis.multipleSourcePairs.size} multiples found. I/O time ${analysis.ioTime}ms, print time ${System.currentTimeMillis() - start}ms")
  }

  private def printClassPathIssues(implicit analysis: ClasspathAnalysis, p: Printer) {
    printHeading("Contested class analysis")
    if (analysis.classConflicts.isEmpty)
      p(s"[CLASS ACT] No contested classes found!")
    else {
      p(s"[WARNING]  ${analysis.classConflicts.map(_._2.size).sum} classes found two or more times in classpath!")
      analysis.classConflictsRanked foreach {
        case (files, classes) =>
          p(s"${classes.size} class conflicts, ${files.size} sources:")
          files map (_.path) foreach { source =>
            p(s"  $source")
          }
          p(s"Contested class(es):")
          (classes groupBy (_.packageName)).toList map {
            case (packidge, cs) => (packidge, cs map (_.simpleName))
          } map {
            case (packidge, cs) =>
              if (cs.size > 2)
                s"  $packidge.{${cs.mkString(" ")}}"
              else
                s"  $packidge.${cs.iterator.next()}"
          } foreach p
          printWinnerAnalysis(files, classes)(_.isInstanceOf[ClassEntry])
          p("")
      }
    }
  }

  private def printPropertiesIssues(implicit analysis: ClasspathAnalysis, p: Printer) {
    printHeading("Contested properties files analysis")
    if (analysis.propertiesConflicts.isEmpty) {
      p("[PROPERTY SOLD] No contested properties files found")
    } else {
      p(s"[WARNING] ${analysis.propertiesConflicts.map(_._2.size).sum} property files found two or more times in classpath!")
      analysis.propertiesConflictsRanked foreach {
        case (files, propertieses) =>
          printSimpleAnalysis(files, propertieses)
          printWinnerAnalysis(files, propertieses)(_.isInstanceOf[PropertiesEntry])
          p("")
      }
    }
  }

  private def printResourcesIssues(implicit analysis: ClasspathAnalysis, p: Printer) {
    printHeading("Contested general files analysis")
    if (analysis.fileConflicts.isEmpty) {
      p("[FILES OK] No contested resources found")
    } else {
      p(s"[WARNING] ${analysis.fileConflicts.map(_._2.size).sum} resource files found two or more times in classpath!")
      analysis.fileConflictsRanked foreach {
        case (files, resourceses) =>
          printSimpleAnalysis(files, resourceses)
          printWinnerAnalysis(files, resourceses)(_.isInstanceOf[FileEntry])
          p("")
      }
    }
  }

  private def printSimpleAnalysis(files: Iterable[FileName], entries: Iterable[PathEntry])(implicit p: Printer) {
    p(s"${entries.size} contested resource paths:")
    (entries groupBy (_.name)).toList map {
      case (name, resources) =>
        p(s"  $name")
    }
    p(s"${files.size} sources:")
    files.map(_.path) foreach { source =>
      p(s"  $source")
    }
  }

  private def printWinnerAnalysis(files: Iterable[FileName], entries: Iterable[PathEntry])(selector: PathEntry => Boolean = entry => true)(implicit analysis: ClasspathAnalysis, p: Printer) {
    val winners = loadersOf(entries)
    p(s"Winner(s):")
    p(s"  ${winners map (_.path) mkString " "}")
    val losers = files filterNot { file =>
      winners exists { winner =>
        winner.path == file.path
      }
    }
    p(s"Loser${if (losers.size > 1) "s" else ""}:")
    winners.toList match {
      case Nil =>
        p("Winner analysis failed")
      case winner :: Nil =>
        losers foreach { loser =>
          analysis.filesVisibleInLoser(winner, loser)(selector) match {
            case None =>
              p(s"  [WARNING] Failed to lookup ${winner.path} / ${loser.path}")
            case Some(Nil) =>
              p(s"  [CLEANING TIME] ${loser.path} contains no visible resources of this type")
            case Some(visible) if visible.size < 5 =>
              p(s"  ${loser.path}: ${visible.size} visible resources: ${visible map (_.name) mkString " "}")
            case Some(visible) =>
              p(s"  ${loser.path}: ${visible.size} visible resources")
          }
        }
      case _ => p("Multiple winners, giving up on analysis!")
    }
  }

  private val allStars =
    "*****************************************************************************************************************"

  private def printHeading(section: String)(implicit p: Printer) {
    val stars = allStars substring (0, section.length + 8)
    p(stars)
    p(s"**  $section  **")
    p(stars)
  }

  private val pathSep = System getProperty "path.separator"

  private def classpath = System getProperty "java.class.path"

  private implicit val cl = Thread.currentThread().getContextClassLoader

  private def loadersOf(classNames: Iterable[PathEntry]): List[FileName] = try {
    (classNames flatMap (_.toContainingFile)).map(FileName(_)).toSet.toList
  } catch {
    case e: Throwable =>
      throw new IllegalStateException(s"Could not load classes", e)
  }
}
