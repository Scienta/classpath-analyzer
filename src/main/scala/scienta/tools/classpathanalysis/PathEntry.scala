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

case class PropertiesEntry(path: String) extends PathEntry(path) {
  override def isClass = false

  override def isProperties = true
}

case class FileEntry(path: String) extends PathEntry(path) {
  override def isClass = false

  override def toResource = name

  override def isProperties = false
}

case class ClassEntry(path: String) extends PathEntry(path.replace("/", ".") dropRight 6) {

  override def isClass = true

  override def toResource = path

  override def isProperties = false

  def packageName = if (name contains ".") name.substring(0, name lastIndexOf ".") else "default"

  def simpleName = name.substring(1 + (name lastIndexOf "."))
}
