package scienta.tools.classpathanalysis.api

import java.io.{OutputStreamWriter, OutputStream, Writer}

import org.slf4j.{LoggerFactory, Logger}
import scienta.tools.classpathanalysis.ClasspathAnalyzer

import scala.io.Codec

class ClasspathReporter(full: Boolean) {

  def writeToLog(): Unit =
    writeToLog(Thread.currentThread().getContextClassLoader, LoggerFactory.getLogger(classOf[ClasspathReporter]))

  def writeToLog(classLoader: ClassLoader): Unit =
    writeToLog(classLoader, LoggerFactory.getLogger(classOf[ClasspathReporter]))

  def writeToLog(classLoader: ClassLoader, logger: Logger): Unit = {
    implicit val writer: String => Unit = logger.info
    implicit val cl = Option(classLoader) getOrElse Thread.currentThread().getContextClassLoader
    ClasspathAnalyzer(full = full)
  }

  def writeTo(reportStream: OutputStream): Unit =
    writeTo(new OutputStreamWriter(reportStream, Codec.UTF8.charSet))

  def writeTo(classLoader: ClassLoader, reportStream: OutputStream): Unit =
    writeTo(classLoader, new OutputStreamWriter(reportStream, Codec.UTF8.charSet))

  def writeTo(reportWriter: Writer): Unit =
    writeTo(Thread.currentThread().getContextClassLoader, reportWriter)

  def writeTo(classLoader: ClassLoader, reportWriter: Writer): Unit = {
    implicit val writer: String => Unit = _ :: "\n" :: Nil foreach reportWriter.write
    implicit val cl = Option(classLoader) getOrElse Thread.currentThread().getContextClassLoader
    ClasspathAnalyzer(full = full)
  }
}
