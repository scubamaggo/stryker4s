package stryker4s.report

import cats.effect.IO
import grizzled.slf4j.Logging
import stryker4s.config.Config
import stryker4s.model._
import stryker4s.run.threshold._

class ConsoleReporter(implicit config: Config) extends FinishedRunReporter with ProgressReporter with Logging {

  private[this] val mutationScoreStr = "Mutation score:"

  override def reportMutationStart(mutant: Mutant): Unit = {
    info(s"Starting test-run ${mutant.id + 1}...")
  }

  override def reportMutationComplete(mutant: MutantRunResult, totalMutants: Int): Unit = {
    val id = mutant.mutant.id + 1
    info(s"Finished mutation run $id/$totalMutants (${((id / totalMutants.toDouble) * 100).round}%)")
  }

  override def reportRunFinished(runResults: MutantRunResults): IO[Unit] = {
    val (detected, rest) = runResults.results partition (_.isInstanceOf[Detected])
    val (undetected, _) = rest partition (_.isInstanceOf[Undetected])

    val detectedSize = detected.size
    val undetectedSize = undetected.size

    val totalMutants = detectedSize + undetectedSize

    def reportScoreStatus(mutationScore: Double): IO[Unit] = {
      ThresholdChecker.determineScoreStatus(mutationScore) match {
        case SuccessStatus => IO(info(s"$mutationScoreStr $mutationScore%"))
        case WarningStatus => IO(warn(s"$mutationScoreStr $mutationScore%"))
        case DangerStatus =>
          IO(error(s"Mutation score dangerously low!"))
          IO(error(s"$mutationScoreStr $mutationScore%"))
        case ErrorStatus =>
          IO(error(
            s"Mutation score below threshold! Score: $mutationScore%. Threshold: ${config.thresholds.break}%"
          ))
      }
    }

    for {
      _ <- IO(info(s"Mutation run finished! Took ${runResults.duration.toSeconds} seconds"))
      _ <- IO(info(s"Total mutants: $totalMutants, detected: $detectedSize, undetected: $undetectedSize"))
      _ <- IO(debug(resultToString("Detected", detected)))
      _ <- IO(info(resultToString("Undetected", undetected)))
      _ <- reportScoreStatus(runResults.mutationScore)
    } yield ()
  }

  private def resultToString(name: String, mutants: Iterable[MutantRunResult]): String =
    s"$name mutants:\n" +
      mutants.toSeq
        .sortBy(_.mutant.id)
        .map(mutantDiff)
        .mkString("\n")

  private def mutantDiff(mrr: MutantRunResult): String = {
    val mutant = mrr.mutant

    val line = mutant.original.pos.startLine + 1
    val col = mutant.original.pos.startColumn + 1

    s"""${mutant.id}. [${mrr.getClass.getSimpleName}]
       |${mrr.fileSubPath}:$line:$col
       |-\t${mutant.original}
       |+\t${mutant.mutated}
       |""".stripMargin
  }
}
