package scienta.tools.classpathanalysis

import java.io.File
import java.net.URI

sealed abstract class PathEntry(val name: String) {

  def isClass: Boolean

  def isNamed(q: String) = name == q

  def toResource: String = name

  def isProperties: Boolean

  def toStream(implicit cl: ClassLoader) = Option(cl getResourceAsStream toResource)

  def toResolvedUrl(implicit cl: ClassLoader) = Option(cl getResource toResource)

  def toContainingFile(implicit cl: ClassLoader): Option[File] =
    try {
      toResolvedUrl map (_.getFile) map unwrapped map asFile
    } catch {
      case e: Throwable => throw new IllegalArgumentException(s"$this failed to locate itself!", e)
    }

  private def unwrapped(file: String) =
    if (file contains ".jar!") s"${file substring(0, file indexOf ".jar!")}.jar"
    else file dropRight toResource.length

  private def asFile(path: String) =
    if (path contains ".jar") new File((URI create path).toURL.getFile)
    else new File(path)
}

case class PropertiesFile(path: String) extends PathEntry(path) {
  override def isClass = false

  override def isProperties = true
}

case class ResourceFile(path: String) extends PathEntry(path) {
  override def isClass = false

  override def toResource = name

  override def isProperties = false
}

case class ClassFile(path: String) extends PathEntry(path.replace("/", ".") dropRight 6) {

  override def isClass = true

  override def toResource = path

  override def isProperties = false

  def packageName = if (name contains ".") name.substring(0, name lastIndexOf ".") else "default"

  def simpleName = name.substring(1 + (name lastIndexOf "."))
}

case class FileName(path: String)
object FileName {
  def apply(file: File): FileName = FileName(file.getPath)
}

class ClasspathAnalysis(pathEntries: Iterable[File]) {

  private def invertPairs[V, K](m: Iterable[(V, K)]): Map[K, Iterable[V]] = {
    def one(pair: (V, K)) = pair._1
    def two(pair: (V, K)) = pair._2
    m groupBy two mapValues (_ map one)
  }

  private def toEntry(resource: String) =
    if (resource endsWith ".class")
      ClassFile(resource)
    else if (resource endsWith ".properties")
      PropertiesFile(resource)
    else
      ResourceFile(resource)

  private def multipleFiles(entrySources: (PathEntry, Iterable[FileName])) = entrySources._2.size > 1

  private val startAnalysisTime = System.currentTimeMillis()

  private val fileClassesTuples: List[(FileName, Iterable[PathEntry])] =
    (pathEntries map LoadableClasspathElement map { source =>
      (FileName(source.file), source.visibleResources() map toEntry)
    }).toList

  val loadTime = System.currentTimeMillis() - startAnalysisTime

  val fileEntries: Map[FileName, Iterable[PathEntry]] = Map(fileClassesTuples: _*)

  val fileEntryPairs: Iterable[(FileName, PathEntry)] = fileClassesTuples flatMap { case (file, names) => names map ((file, _))}

  val multipleSourcePairs: Map[PathEntry, Iterable[FileName]] = invertPairs(fileEntryPairs) filter multipleFiles

  private def multipleSources[T](relevant: PathEntry => Boolean): Map[PathEntry, Iterable[FileName]] =
    multipleSourcePairs filter { case (entry, _) => relevant(entry)}

  // Make a map from class name to the sorted list of files that provided it, and keep only lists with multiple elements
  val multipleSourceClasses: Map[ClassFile, Iterable[FileName]] =
    multipleSources(_.isInstanceOf[ClassFile]).asInstanceOf[Map[ClassFile, Iterable[FileName]]]

  // Make a map from class name to the sorted list of files that provided it, and keep only lists with multiple elements
  val multipleSourceFiles: Map[ResourceFile, Iterable[FileName]] =
    multipleSources(_.isInstanceOf[ResourceFile]).asInstanceOf[Map[ResourceFile, Iterable[FileName]]]

  // Make a map from class name to the sorted list of files that provided it, and keep only lists with multiple elements
  val multipleSourceProperties: Map[PropertiesFile, Iterable[FileName]] =
    multipleSources(_.isInstanceOf[PropertiesFile]).asInstanceOf[Map[PropertiesFile, Iterable[FileName]]]

  // Make a map from sorted lists of files to all class names they have in common
  val classConflicts: Map[Iterable[FileName], Iterable[ClassFile]] = invertPairs(multipleSourceClasses)

  // Make a map from sorted lists of files to all class names they have in common
  val resourceConflicts: Map[Iterable[FileName], Iterable[ResourceFile]] = invertPairs(multipleSourceFiles)

  // Make a map from sorted lists of files to all class names they have in common
  val propertiesConflicts: Map[Iterable[FileName], Iterable[PropertiesFile]] = invertPairs(multipleSourceProperties)

  val analysisTime: Long = System.currentTimeMillis() - startAnalysisTime

  def classConflictsRanked = classConflicts.toList.sortBy(_._2.size).reverse

  def propertiesConflictsRanked = propertiesConflicts.toList.sortBy(_._2.size).reverse

  def resourceConflictsRanked = resourceConflicts.toList.sortBy(_._2.size).reverse

  def filesVisibleInLoser(winner: FileName, loser: FileName)(selector: PathEntry => Boolean = entry => true): Option[Iterable[PathEntry]] =
    (fileEntries get winner map(_ filter selector) , fileEntries get loser map (_ filter selector)) match {
      case (Some(winnerResources), Some(loserResources)) =>
        def isIn(entries: Iterable[PathEntry])(entry: PathEntry) = winnerResources exists (entry == _)
        Some(loserResources filterNot isIn(winnerResources))
      case _ => None
    }
}

object ClasspathAnalyzer {

  type Printer = (String) => Unit

  def analyze(implicit p: Printer): Unit = {
    implicit val analysis = new ClasspathAnalysis(classpath split pathSep map (new File(_)))

    printClassPathIssues
    p("")
    printPropertiesIssues(analysis, p)
    p("")
    printResourcesIssues(analysis, p)
    p("")
    p(s"${analysis.fileEntryPairs.size} resources analyzed in ${analysis.analysisTime}ms, ${analysis.multipleSourcePairs.size} multiples found. I/O time ${analysis.loadTime}ms")
  }

  private def printClassPathIssues(implicit analysis: ClasspathAnalysis, p: Printer) {
    if (analysis.classConflicts.isEmpty)
      p(s"[CLASS ACT] No contested classes found!")
    else {
      p(s"***********************************")
      p(s"[WARNING] Contested class analysis: ${analysis.classConflicts.map(_._2.size).sum} classes found two or more times in classpath!")
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
          printWinnerAnalysis(files, classes)(_.isInstanceOf[ClassFile])
          p("")
      }
    }
  }

  private def printPropertiesIssues(implicit analysis: ClasspathAnalysis, p: Printer) {
    if (analysis.propertiesConflicts.isEmpty) {
      p("[PROPERTY SOLD] No contested properties files found")
    } else {
      p(s"********************************************")
      p(s"[WARNING] Contested property files analysis: ${analysis.propertiesConflicts.map(_._2.size).sum} property files found two or more times in classpath!")
      analysis.propertiesConflictsRanked foreach {
        case (files, propertieses) =>
          printSimpleAnalysis(files, propertieses)
          printWinnerAnalysis(files, propertieses)(_.isInstanceOf[PropertiesFile])
          p("")
      }
    }
  }

  private def printResourcesIssues(implicit analysis: ClasspathAnalysis, p: Printer) {
    if (analysis.resourceConflicts.isEmpty) {
      p("[PROPERTY SOLD] No contested resources found")
    } else {
      p(s"********************************************")
      p(s"[WARNING] Contested resources analysis: ${analysis.resourceConflicts.map(_._2.size).sum} resource files found two or more times in classpath!")
      analysis.resourceConflictsRanked foreach {
        case (files, resourceses) =>
          printSimpleAnalysis(files, resourceses)
          printWinnerAnalysis(files, resourceses)(_.isInstanceOf[ResourceFile])
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
