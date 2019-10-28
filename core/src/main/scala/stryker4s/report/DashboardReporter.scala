package stryker4s.report

import cats.effect.IO
import cats.syntax.apply._
import grizzled.slf4j.Logging
import scalaj.http.HttpResponse
import stryker4s.http.{HttpClient, WebIO}
import stryker4s.model.MutantRunResults
import stryker4s.report.dashboard.Providers._
import stryker4s.report.mapper.MutantRunResultMapper
import stryker4s.report.model.StrykerDashboardReport

class DashboardReporter(webIO: WebIO, ciEnvironment: CiEnvironment)
    extends FinishedRunReporter
    with MutantRunResultMapper
    with Logging {

  private val dashboardRootURL: String = "https://dashboard.stryker-mutator.io"
  private val dashboardURL: String = s"$dashboardRootURL/api/reports"

  def buildScoreResult(input: MutantRunResults): StrykerDashboardReport = {
    StrykerDashboardReport(
      ciEnvironment.apiKey,
      ciEnvironment.repositorySlug,
      ciEnvironment.branchName,
      input.mutationScore
    )
  }

  def writeReportToDashboard(url: String, report: StrykerDashboardReport): IO[HttpResponse[String]] = {
    IO(webIO.postRequest(url, report.toJson))
  }

  override def reportRunFinished(runResults: MutantRunResults): IO[Unit] = {
    writeReportToDashboard(dashboardURL, buildScoreResult(runResults)).map {
      case response if response.code == 200 =>
        IO(info(s"Sent report to dashboard: $dashboardRootURL"))
      case response =>
        IO(error(s"Failed to send report to dashboard.")) *>
          IO(error(s"Expected status code 201, but was ${response.code}. Body: '${response.body}'"))
    }
  }
}

object DashboardReporter {

  def resolveProvider(): Option[DashboardReporter] =
    resolveCiEnvironment()
      .map(new DashboardReporter(HttpClient, _))

  def resolveCiEnvironment(): Option[CiEnvironment] =
    tryResolveEnv(TravisProvider) orElse
      tryResolveEnv(CircleProvider)

  def tryResolveEnv(provider: CiProvider): Option[CiEnvironment] =
    if (provider.isPullRequest) None
    else
      for {
        apiKey <- provider.determineApiKey()
        branchName <- provider.determineBranch()
        repoName <- provider.determineRepository()
        repositorySlug = "github.com/" + repoName
      } yield CiEnvironment(apiKey, repositorySlug, branchName)

}

case class CiEnvironment(apiKey: String, repositorySlug: String, branchName: String)
