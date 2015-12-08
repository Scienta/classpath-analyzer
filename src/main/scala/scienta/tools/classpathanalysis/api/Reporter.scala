package scienta.tools.classpathanalysis.api

import java.io.{OutputStreamWriter, OutputStream, Writer}

import scienta.tools.classpathanalysis.ClasspathAnalyzer

import scala.io.Codec

class Reporter {

  def writeTo(reportStream: OutputStream): Unit =
    writeTo(new OutputStreamWriter(reportStream, Codec.UTF8.charSet))

  def writeTo(classLoader: ClassLoader, reportStream: OutputStream): Unit =
    writeTo(classLoader, new OutputStreamWriter(reportStream, Codec.UTF8.charSet))

  def writeTo(reportWriter: Writer): Unit =
    writeTo(Thread.currentThread().getContextClassLoader, reportWriter)

  def writeTo(classLoader: ClassLoader, reportWriter: Writer): Unit = {
    implicit val writer: String => Unit = _ :: "\n" :: Nil foreach reportWriter.write
    implicit val cl = classLoader
    ClasspathAnalyzer()
  }
}
