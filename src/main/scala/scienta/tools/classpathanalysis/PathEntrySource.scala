package scienta.tools.classpathanalysis

import java.io.File
import java.util.jar.JarFile

import scala.collection.JavaConversions.enumerationAsScalaIterator

/**
 * A source of class file resources.  Can be a jar file or a class file.
 */
case class PathEntrySource(file: File) {

  /**
    * @return A list of class resources, which abstract the reading of class data.
    */
  def visibleResources(): Iterable[PathEntry] = {
    val strings: Iterable[String] =
      if (isJarFile) jarFileEntries(file)
      else if (isInJarFile) jarFileEntries(jarPath(file))
      else pathEntries(file)
    strings map toEntry
  }

  private def isJarFile = file.getName endsWith ".jar"

  private def isInJarFile = file.getAbsolutePath.contains(".jar!")

  /**
    * Get class resources found in a jar file.
    *
    * @param file Jar file
    * @return Class resources
    */
  private def jarFileEntries(file: File): Iterable[String] =
    new JarFile(file).entries().toList filterNot (_.isDirectory) map (_.getName)

  private def jarPath(file: File): File = {
    val name = file.getPath.substring(0, file.getPath indexOf ".jar!") + ".jar"
    new File(if (name startsWith "file:") name substring "file:".length else name)
  }

  private def toEntry(resource: String) =
    if (resource endsWith ".class")
      ClassEntry(resource)
    else if (resource endsWith ".properties")
      PropertiesEntry(resource)
    else
      FileEntry(resource)

  /**
    * Get class resources found in a directory.
    *
    * @param root Root directory
    * @return Class resources
    */
  private def pathEntries(root: File) =
    if (file.isDirectory)
      try {
        val rootLength = math.max(0, root.getAbsolutePath.length + 1)
        filesIn(root) filterNot (_.getAbsolutePath.isEmpty) map (_.getAbsolutePath substring rootLength)
      } catch {
        case e: Throwable =>
          Nil
      }
    else
      List(file.getAbsolutePath)

  /**
   * Get all files below a given directory
   * @param path Root directory
   * @return All files below it
   */
  private def filesIn(path: File): Iterable[File] = {
    if (path.isFile) List(path) else listPaths(path) flatMap filesIn
  }

  private def listPaths(file: File) = file.listFiles() filterNot (_.getName startsWith ".")
}
