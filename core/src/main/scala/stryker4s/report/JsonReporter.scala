package stryker4s.report

import better.files.File
import cats.effect.IO
import cats.syntax.apply._
import grizzled.slf4j.Logging
import stryker4s.config.Config
import stryker4s.files.FileIO
import stryker4s.model.MutantRunResults
import stryker4s.report.mapper.MutantRunResultMapper
import stryker4s.report.model.MutationTestReport

class JsonReporter(fileIO: FileIO)(implicit config: Config)
    extends FinishedRunReporter
    with MutantRunResultMapper
    with Logging {

  private def buildScoreResult(report: MutantRunResults): MutationTestReport = {
    toReport(report)
  }

  def writeReportJsonTo(file: File, report: MutantRunResults): IO[Unit] =
    for {
      content <- IO.pure(buildScoreResult(report))
      _ <- IO(fileIO.createAndWrite(file, content.toJson))
    } yield ()

  override def reportRunFinished(runResults: MutantRunResults): IO[Unit] = {
    val targetLocation = config.baseDir / s"target/stryker4s-report-${runResults.timestamp}"
    val resultLocation = targetLocation / "report.json"

    writeReportJsonTo(resultLocation, runResults) *>
       IO(info(s"Written JSON report to $resultLocation"))
  }
}
