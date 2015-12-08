package scienta.tools.classpathanalysis

import java.io.File
import java.net.URI

import org.slf4j.Logger

import scala.annotation.tailrec
import scala.reflect.internal.util.ScalaClassLoader.URLClassLoader
import scala.language.postfixOps

class ClasspathAnalyzer(classLoader: ClassLoader, full: Boolean = false) {

  private def invertAndMap[V, K](m: Iterable[(V, K)]): Map[K, Iterable[V]] = m groupBy (_._2) mapValues (_ map (_._1))

  private val startTime = System.currentTimeMillis()

  @tailrec
  private def toHomeDir(file: File): File = {
    if (isJavaHome(file)) file
    else if (file.getParentFile == null) null
    else toHomeDir(file.getParentFile)
  }

  private def isJavaHome(file: File) =
    new File(file, "lib").exists() && new File(file, "bin").exists && new File(new File(file, "bin"), "java").exists

  val javaHome = {
    val url = (ClassLoader.getSystemClassLoader getResource "java/lang/Object.class").getFile
    val jarFile = new File((URI create s"${url.substring(0, url indexOf ".jar!")}.jar").toURL.getFile)
    Option(toHomeDir(jarFile)) getOrElse new File(System getProperty "java.home")
  }

  private val ioStartTime = System.currentTimeMillis()

  private val files: Iterable[File] = classLoader match {
    case loader if loader == ClassLoader.getSystemClassLoader =>
      System getProperty "java.class.path" split (System getProperty "path.separator") map (new File(_))
    case loader: URLClassLoader =>
      loader.getURLs.toList map (_.getFile) map (new File(_))
    case _ => Nil
  }

  private val fileClassesTuples: Iterable[(FileName, Iterable[PathEntry])] = files map PathEntrySource map { source =>
    (FileName(source.file), source.visibleResources())
  }

  val ioTime = System.currentTimeMillis() - ioStartTime

  val fileEntries: Map[FileName, Iterable[PathEntry]] = Map(fileClassesTuples.toSeq: _*)

  val fileEntryPairs: Iterable[(FileName, PathEntry)] =
    fileClassesTuples flatMap { case (file, entries) => entries map ((file, _))}

  private val entrySourcesPairs: Map[PathEntry, Iterable[FileName]] = invertAndMap(fileEntryPairs)

  val multipleSourcePairs: Map[PathEntry, Iterable[FileName]] = entrySourcesPairs filter (_._2.size > 1)

  private def relevantClass(entry: PathEntry) = entry.isInstanceOf[ClassEntry] && !(entry.name contains "$")

  val allClassNames = (fileEntryPairs filter { pair => relevantClass(pair._2) } map (_._2.name)).toSet

  private def isServiceLike(file: FileEntry) = allClassNames contains file.fileName

  private def relevantFile (entry: PathEntry) =
    entry.isInstanceOf[FileEntry] && noDisqualifiers(irrelevantFiles)(entry.name)

  private def relevantProperties (entry: PathEntry) =
    entry.isInstanceOf[PropertiesEntry] && noDisqualifiers(irrelevantProperties)(entry.name)

  private val irrelevantFiles =
    Set[String => Boolean](
      _ == "META-INF/MANIFEST.MF",
      _ == "META-INF/INDEX.LIST",
      _.toLowerCase == "LICENSE.txt")

  private val irrelevantProperties =
    Set[String => Boolean](
      _ endsWith "pom.properties",
      _ contains "Messages",
      _ startsWith "sun/tools/",
      _ startsWith "sun/rmi/",
      _ startsWith "com/sun/",
      _ startsWith "com/oracle",
      _ startsWith "jdk/nashorn",
      _ startsWith "jdk/internal")

  private def noDisqualifiers[T](funs: Iterable[T => Boolean])(t: T): Boolean = !(funs exists (_(t)))

  val multipleSourceClasses: Map[ClassEntry, Iterable[FileName]] =
    (multipleSourcePairs filterKeys relevantClass).asInstanceOf[Map[ClassEntry, Iterable[FileName]]]

  val multipleSourceProperties: Map[PropertiesEntry, Iterable[FileName]] =
    (multipleSourcePairs filterKeys relevantProperties).asInstanceOf[Map[PropertiesEntry, Iterable[FileName]]]

  val allSourceProperties: Map[PropertiesEntry, Iterable[FileName]] =
    (entrySourcesPairs filterKeys relevantProperties).asInstanceOf[Map[PropertiesEntry, Iterable[FileName]]]

  val multipleSourceFiles: Map[FileEntry, Iterable[FileName]] =
    (multipleSourcePairs filterKeys relevantFile).asInstanceOf[Map[FileEntry, Iterable[FileName]]]

  val allSourceFiles: Map[FileEntry, Iterable[FileName]] =
    (entrySourcesPairs filterKeys relevantFile).asInstanceOf[Map[FileEntry, Iterable[FileName]]]

  val classConflicts: Map[Iterable[FileName], Iterable[ClassEntry]] = invertAndMap(multipleSourceClasses)

  val propertiesConflicts: Map[Iterable[FileName], Iterable[PropertiesEntry]] = invertAndMap(multipleSourceProperties)

  val fileConflicts: Map[Iterable[FileName], Iterable[FileEntry]] = invertAndMap(multipleSourceFiles)

  def rank[E <: PathEntry](conflicts: Map[Iterable[FileName], Iterable[E]]) = conflicts.toList.sortBy(_._2.size).reverse

  val classConflictsRanked = rank(classConflicts)

  val propertiesConflictsRanked = rank(propertiesConflicts)

  val fileConflictsRanked = rank(fileConflicts)

  val conflictyClassSourcesRanked: List[(FileName, Iterable[FileName])] =
    (classConflicts.keys.flatten.toSet.toList map ((file: FileName) => {
      (file, (classConflicts.keys map (_.toSet) filter (_ contains file)).flatten filterNot (_ == file))
    })).sortBy(_._2.size).reverse

  val time = System.currentTimeMillis() - startTime

  def filesVisibleInLoser(winner: FileName, loser: FileName)(selector: PathEntry => Boolean = entry => true): Option[Iterable[PathEntry]] =
    (fileEntries get winner map(_ filter selector) , fileEntries get loser map (_ filter selector)) match {
      case (Some(winnerResources), Some(loserResources)) =>
        def isIn(entries: Iterable[PathEntry])(entry: PathEntry) = winnerResources exists (entry == _)
        Some(loserResources filterNot isIn(winnerResources))
      case _ => None
    }
}

object ClasspathAnalyzer {

  private val tcl = Thread.currentThread().getContextClassLoader

  type Printer = (String) => Unit

  def stdout(): Unit = stdout(tcl)

  def stdout(cl: ClassLoader) = apply(full = false)(cl, println)

  def stdout(full: Boolean): Unit = stdout(full = false, tcl)

  def stdout(full: Boolean, cl: ClassLoader) = apply(full)(cl, println)

  def logInfo(logger: Logger): Unit = logInfo(full = false, tcl, logger)

  def logInfo(cl: ClassLoader, logger: Logger) = if (logger.isInfoEnabled) apply(full = false)(cl, logger info)

  def logDebug(logger: Logger): Unit = logDebug(full = false, tcl, logger)

  def logDebug(cl: ClassLoader, logger: Logger) = if (logger.isDebugEnabled) apply(full = false)(cl, logger debug)

  def logInfo(full: Boolean, logger: Logger): Unit = logInfo(full, tcl, logger)

  def logInfo(full: Boolean, cl: ClassLoader, logger: Logger) = if (logger.isInfoEnabled) apply(full)(cl, logger info)

  def logDebug(full: Boolean, logger: Logger): Unit = logDebug(full, tcl, logger)

  def logDebug(full: Boolean, cl: ClassLoader, logger: Logger) = if (logger.isDebugEnabled) apply(full)(cl, logger debug)

  def main(args: Array[String]) = ClasspathAnalyzer(full = false)(classLoader = tcl, p = println)

  def apply(full: Boolean = false)(implicit classLoader: ClassLoader, p: Printer): Unit = {
    implicit val analysis = new ClasspathAnalyzer(Option(classLoader) getOrElse tcl)
    val start = System.currentTimeMillis()
    print(full)
    p(s"${
      analysis.fileEntryPairs.size
    } resources analyzed in ${
      analysis.time
    }ms, ${
      analysis.multipleSourcePairs.size
    } multiples found. I/O time ${
      analysis.ioTime
    }ms, print time ${
      System.currentTimeMillis() - start
    }ms")
  }

  private def print(full: Boolean)(implicit analysis: ClasspathAnalyzer, cl: ClassLoader, p: Printer) {
    printClassPathIssues
    if (full) {
      p("")
      printPropertiesIssues
      p("")
      printResourcesIssues
      p("")
      printServices
      p("")
      printVisibleProperties
    }
  }

  private def printServices(implicit analysis: ClasspathAnalyzer, cl: ClassLoader, p: Printer) {
    printHeading("Visible service factory configurations")
    (analysis.allSourceFiles filter { pair => analysis.isServiceLike(pair._1) }).keys foreach { entry =>
      validFile(entry) foreach { file =>
        p(s"Service: ${entry.name}")
        p(s"  Loader: $file")
        val load: String = entry.load
        if (load.length > 256)
          p(s"  Contents: <${load.length} chars>")
        else {
          printLines("  ", load)
        }
      }
    }
  }

  private def printVisibleProperties(implicit analysis: ClasspathAnalyzer, cl: ClassLoader, p: Printer) {
    printHeading("Visible properties")
    analysis.allSourceProperties foreach {
      case (entry, files) =>
        validFile(entry) foreach { file =>
          p(s"${entry.name}:")
          p(s"  Loader: $file")
          p(s"  Contents:")
          val content: Map[String, String] = entry.load
          content take 20 foreach {
            case (key, value) =>
              printLines(s"    $key=", value)
          }
          if (content.size > 20) p(s"    <${content.size} properties total>...")
          p("")
        }
    }
  }

  private def sensibleLength(v: String): String =
    if (v == null || v.length < 80) v
    else s"${v.substring(0, 70)}...<${v.length} total>...${v.substring(v.length - 7, v.length)}"

  private def printLines(header: String, value: String)(implicit p: Printer) {
    val indent = header.length
    if (value == null || value.isEmpty) {
      p(s"$header")
    } else if (value contains (System getProperty "line.separator")) {
      val lines = value split (System getProperty "line.separator")
      if (lines.isEmpty) {
        p(s"$header${sensibleLength(value)}")
      } else {
        p(s"$header|${sensibleLength(lines(0))}")
        if (lines.length > 1) {
          lines.tail foreach { line =>
            p(s"${indent spaces()}|$line}")}
        }
      }
    } else {
      p(s"$header${sensibleLength(value)}")
    }
  }

  case class SpaciousInt(i: Int) {
    def times(s: String) = repeated(s, i)
    def spaces(): Unit = times(" ")
  }

  private implicit def int2SpaciousInt(i: Int): SpaciousInt = SpaciousInt(i)

  private def repeated(str: String, countEm: Int) = (Stream continually str take countEm).mkString

  private def validFile(entry: PathEntry)(implicit cl: ClassLoader): Option[File] =
    entry.toContainingFile filterNot { _.getName contains "org.scala-lang" }

  private def printClassPathIssues(implicit analysis: ClasspathAnalyzer, cl: ClassLoader, p: Printer) {
    printHeading("Contested class analysis")
    if (analysis.classConflicts.isEmpty)
      p(s"[CLASS ACT] No contested classes found!")
    else {
      p(s"[WARNING] ${analysis.classConflicts.map(_._2.size).sum} classes found two or more times in classpath!")
      analysis.classConflictsRanked foreach {
        case (files, classes) =>
          p(s"${classes.size} class conflicts found among ${files.size} sources:")
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
      p("Most conflict-prone sources:")
      analysis.conflictyClassSourcesRanked foreach {
        case (file, conflicts) =>
          p(s"${conflicts.size} conflict(s): ${file.path}")
          p(s"  [${conflicts map (_.path) mkString " "}]")
      }
    }
  }

  private def printPropertiesIssues(implicit analysis: ClasspathAnalyzer, cl: ClassLoader, p: Printer) {
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

  private def printResourcesIssues(implicit analysis: ClasspathAnalyzer, cl: ClassLoader, p: Printer) {
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
    (entries groupBy (_.name)).toList foreach {
      case (name, resources) =>
        p(s"  $name")
    }
    p(s"${files.size} sources:")
    files.map(_.path) foreach { source =>
      p(s"  $source")
    }
  }

  private def printWinnerAnalysis(files: Iterable[FileName], entries: Iterable[PathEntry])(selector: PathEntry => Boolean = entry => true)(implicit analysis: ClasspathAnalyzer, cl: ClassLoader, p: Printer) {
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
              p(s"  [CLEANING TIME] ${loser.path} is living in the shadows")
            case Some(visible) if visible.size < 5 =>
              p(s"  ${loser.path}: ${visible.size} visible resources: ${visible map (_.name) mkString " "}")
            case Some(visible) =>
              p(s"  ${loser.path}: ${visible.size} visible resources")
          }
        }
      case _ => p("Multiple winners, giving up on analysis!")
    }
  }

  private def printHeading(section: String)(implicit p: Printer) {
    val stars = (section.length + 8) times "*"
    p(stars)
    p(s"**  $section  **")
    p(stars)
  }

  private def loadersOf(entries: Iterable[PathEntry])(implicit cl: ClassLoader): List[FileName] = try {
    (entries flatMap (_.toContainingFile)).map(FileName(_)).toSet.toList
  } catch {
    case e: Throwable =>
      throw new IllegalStateException(s"Could not load classes", e)
  }
}
