package stryker4s.report

import cats.effect.IO
import stryker4s.config.Config
import stryker4s.model.{Mutant, MutantRunResult, MutantRunResults}
import stryker4s.scalatest.LogMatchers
import stryker4s.testutil.{MockitoSuite, Stryker4sSuite}

import scala.concurrent.duration._
import scala.language.postfixOps
import scala.reflect.ClassTag

class ReporterTest extends Stryker4sSuite with MockitoSuite with LogMatchers {

  private def successfulReporterMock[T <: FinishedRunReporter: ClassTag]: T = {
    val m = mock[T]
    when(m.reportRunFinished(any[MutantRunResults])).thenReturn(IO.unit)
    m
  }

  private def failingReporterMock[T <: FinishedRunReporter: ClassTag](e: Throwable): T = {
    val m = mock[T]
    when(m.reportRunFinished(any[MutantRunResults])).thenReturn(IO.raiseError(e))
  }

  describe("reporter") {

    it("should log that the console reporter is used when a non existing reporter is configured") {
      val consoleReporterMock = successfulReporterMock[ConsoleReporter]
      implicit val config: Config = Config()

      val mutantRunResults = MutantRunResults(List.empty, 100.0, 10 seconds)

      val sut: Reporter = new Reporter() {
        override lazy val reporters: List[ConsoleReporter] = List(consoleReporterMock)
      }

      sut.reportRunFinished(mutantRunResults).unsafeRunSync()

      verify(consoleReporterMock).reportRunFinished(mutantRunResults)
    }

    describe("reportMutationStart") {
      it("should report to all progressReporters that a mutation run is started.") {
        val mutantMock = mock[Mutant]
        val consoleReporterMock = mock[ConsoleReporter]
        val progressReporterMock = mock[ProgressReporter]

        implicit val config: Config = Config(reporters = Nil)

        val sut: Reporter = new Reporter() {
          override lazy val reporters: List[MutationRunReporter] = List(consoleReporterMock, progressReporterMock)
        }

        sut.reportMutationStart(mutantMock)

        verify(consoleReporterMock).reportMutationStart(mutantMock)
        verify(progressReporterMock).reportMutationStart(mutantMock)
      }

      it("Should not report to finishedRunReporters that is mutation run is started.") {
        val consoleReporterMock = mock[ConsoleReporter]
        val finishedRunReporterMock = mock[FinishedRunReporter]
        val mutantMock = mock[Mutant]

        implicit val config: Config = Config()

        val sut: Reporter = new Reporter() {
          override lazy val reporters: List[MutationRunReporter] = List(consoleReporterMock, finishedRunReporterMock)
        }

        sut.reportMutationStart(mutantMock)

        verify(consoleReporterMock).reportMutationStart(mutantMock)
        verifyZeroInteractions(finishedRunReporterMock)
      }
    }

    describe("reportMutationComplete") {
      it("should report to all progressReporters that a mutation run is completed") {
        val mutantRunResultMock = mock[MutantRunResult]
        val consoleReporterMock = mock[ConsoleReporter]
        val progressReporterMock = mock[ProgressReporter]

        implicit val config: Config = Config(reporters = Nil)

        val sut: Reporter = new Reporter() {
          override lazy val reporters: List[ProgressReporter] = List(consoleReporterMock, progressReporterMock)
        }

        sut.reportMutationComplete(mutantRunResultMock, 1)

        verify(consoleReporterMock).reportMutationComplete(mutantRunResultMock, 1)
        verify(progressReporterMock).reportMutationComplete(mutantRunResultMock, 1)
      }

      it("should not report to finishedMutationRunReporters that a mutation run is completed") {
        val consoleReporterMock = mock[ConsoleReporter]
        val finishedRunReporterMock = mock[FinishedRunReporter]
        val mutantRunResultMock = mock[MutantRunResult]

        implicit val config: Config = Config()

        val sut: Reporter = new Reporter() {
          override lazy val reporters: List[MutationRunReporter] = List(consoleReporterMock, finishedRunReporterMock)
        }

        sut.reportMutationComplete(mutantRunResultMock, 1)

        verify(consoleReporterMock).reportMutationComplete(mutantRunResultMock, 1)
        verifyZeroInteractions(finishedRunReporterMock)
      }
    }

    describe("reportRunFinished") {
      it("should report to all finished mutation run reporters that a mutation run is completed") {
        val consoleReporterMock = successfulReporterMock[ConsoleReporter]
        val FinishedRunReporterMock = successfulReporterMock[FinishedRunReporter]
        implicit val config: Config = Config()

        val mutantRunResults = MutantRunResults(List.empty, 100.0, 10 seconds)

        val sut: Reporter = new Reporter() {
          override lazy val reporters: List[MutationRunReporter] = List(consoleReporterMock, FinishedRunReporterMock)
        }

        sut.reportRunFinished(mutantRunResults).unsafeRunSync()

        verify(consoleReporterMock).reportRunFinished(mutantRunResults)
        verify(FinishedRunReporterMock).reportRunFinished(mutantRunResults)
      }

      it("should not report a finished mutation run to a progress reporter") {
        val consoleReporterMock = successfulReporterMock[ConsoleReporter]
        val progressReporterMock = mock[ProgressReporter]
        implicit val config: Config = Config()

        val mutantRunResults = MutantRunResults(List.empty, 100.0, 10 seconds)

        val sut: Reporter = new Reporter() {
          override lazy val reporters: List[MutationRunReporter] = List(consoleReporterMock, progressReporterMock)
        }

        sut.reportRunFinished(mutantRunResults).unsafeRunSync()

        verify(consoleReporterMock).reportRunFinished(mutantRunResults)
        verifyZeroInteractions(progressReporterMock)
      }

      it("should still call other reporters if a reporter throws an exception") {
        val consoleReporterMock = failingReporterMock[ConsoleReporter](new RuntimeException("Something happened"))
        val progressReporterMock = successfulReporterMock[FinishedRunReporter]
        implicit val config: Config = Config()

        val mutantRunResults = MutantRunResults(List.empty, 100.0, 10 seconds)

        val sut: Reporter = new Reporter() {
          override lazy val reporters: List[MutationRunReporter] = List(consoleReporterMock, progressReporterMock)
        }

        sut.reportRunFinished(mutantRunResults).unsafeRunSync()

        verify(progressReporterMock).reportRunFinished(mutantRunResults)
      }

      describe("logging") {
        val failedToReportMessage = "1 reporter(s) failed to report:"
        val exceptionMessage = "java.lang.RuntimeException: Something happened"

        val progressReporterMock = mock[ProgressReporter]
        implicit val config: Config = Config()

        val mutantRunResults = MutantRunResults(List.empty, 100.0, 10 seconds)

        it("should log if a report throws an exception") {
          val consoleReporterMock = failingReporterMock[ConsoleReporter](new RuntimeException("Something happened"))
          val sut: Reporter = new Reporter() {
            override lazy val reporters: List[MutationRunReporter] = List(consoleReporterMock, progressReporterMock)
          }

          sut.reportRunFinished(mutantRunResults).unsafeRunSync()

          failedToReportMessage shouldBe loggedAsWarning
          exceptionMessage shouldBe loggedAsWarning
        }

        it("should not log warnings if no exceptions occur") {
          val consoleReporterMock = successfulReporterMock[ConsoleReporter]
          val sut: Reporter = new Reporter() {
            override lazy val reporters: List[MutationRunReporter] = List(consoleReporterMock, progressReporterMock)
          }

          sut.reportRunFinished(mutantRunResults).unsafeRunSync()

          verify(consoleReporterMock).reportRunFinished(mutantRunResults)
          failedToReportMessage should not be loggedAsWarning
          exceptionMessage should not be loggedAsWarning
        }
      }
    }
  }
}
