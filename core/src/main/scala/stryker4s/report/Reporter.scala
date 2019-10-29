package stryker4s.report

import cats.effect.IO
import cats.syntax.traverse._
import cats.instances.list._
import grizzled.slf4j.Logging
import stryker4s.config._
import stryker4s.files.DiskFileIO
import stryker4s.model.{Mutant, MutantRunResult, MutantRunResults}

class Reporter(implicit config: Config) extends FinishedRunReporter with ProgressReporter with Logging {

  lazy val reporters: List[MutationRunReporter] = config.reporters map {
    case ConsoleReporterType             => new ConsoleReporter()
    case HtmlReporterType                => new HtmlReporter(DiskFileIO)
    case JsonReporterType                => new JsonReporter(DiskFileIO)
    case DashboardReporterType(reporter) => reporter
  }

  private[this] val progressReporters = reporters collect { case r: ProgressReporter       => r }
  private[this] val finishedRunReporters = reporters collect { case r: FinishedRunReporter => r }

  override def reportMutationStart(mutant: Mutant): Unit =
    progressReporters.foreach(_.reportMutationStart(mutant))

  override def reportMutationComplete(result: MutantRunResult, totalMutants: Int): Unit =
    progressReporters.foreach(_.reportMutationComplete(result, totalMutants))

  override def reportRunFinished(runResults: MutantRunResults): IO[Unit] = {
    finishedRunReporters
      .traverse(reporter => reporter.reportRunFinished(runResults).attempt)
      .map { results =>
        val exceptions = results.collect { case Left(e) => e }
        if (exceptions.nonEmpty) {
          warn(s"${exceptions.length} reporter(s) failed to report:")
          exceptions.foreach(warn(_))
        }
      }
  }

}
