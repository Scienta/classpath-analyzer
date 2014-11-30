package scienta.tools.classpathanalysis

import java.io.File

case class FileName(path: String)

object FileName {
  def apply(file: File): FileName = FileName(file.getPath)
}
