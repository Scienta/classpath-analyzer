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
  def visibleResources(): Iterable[String] = (
    if (isJarFile) jarFileEntries(file)
    else if (isInJarFile) jarFileEntries(jarPath(file))
    else pathEntries(file)
    ) filterNot internalClass

  private def isJarFile = file.getName endsWith ".jar"

  private def isInJarFile = file.getAbsolutePath.contains(".jar!")

  private def internalClass(resource: String) = (resource contains ".class") && (resource contains "$")

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

  /**
   * Get class resources found in a directory.
   *
   * @param root Root directory
   * @return Class resources
   */
  private def pathEntries(root: File) = {
    val rootLength = root.getAbsolutePath.length + 1
    classFiles(root) map (_.getAbsolutePath substring rootLength)
  }

  /**
   * Get all files below a given directory
   * @param path Root directory
   * @return All files below it
   */
  private def classFiles(path: File): Iterable[File] = {
    if (path.isFile) List(path) else listPaths(path) flatMap classFiles
  }

  private def listPaths(file: File) = file.listFiles() filterNot (_.getName startsWith ".")
}
