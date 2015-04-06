package qirx

import io.WatchService
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.StandardOpenOption.CREATE_NEW
import qirx.io.DefaultWatchService
import qirx.io.WatchService.Create
import scala.concurrent.ExecutionContext
import psp.std._

class Qirx(dir: Path, configuration:Qirx.Configuration) {

  def this(dir: Path, failureHandler: FailureHandler)(implicit ec: ExecutionContext) =
    this(dir, Qirx configurationWith failureHandler)

  private[this] val qirxFile = dir.getFileSystem.getPathMatcher("glob:**.qirx")

  configuration.watchService.watch(dir, {
    case Create(file) if qirxFile matches file  =>
      val rawFileName = file.toFile.getName + ".raw" // they also have file.getFileName.toString
      val rawFile     = file resolveSibling rawFileName

      val contents = new String(Files.readAllBytes(file), UTF_8)

      //val result = configuration.parser.parse(contents)

      // TODO Instead of writing to one file. Think about writing each element to it's own file
      //      what would be the benefits?
      //Files.write(rawFile, result.toString getBytes UTF_8, CREATE_NEW)
    case _ => // ignore
  })
}

object Qirx {
  case class Configuration(
    watchService : WatchService//,
    //parser       : Parser
  )
  def configurationWith(failureHandler: FailureHandler)(implicit ec: ExecutionContext):Configuration =
    Configuration(
      watchService = new DefaultWatchService(failureHandler)//,
      //parser       = Parser.Default
    )
}
