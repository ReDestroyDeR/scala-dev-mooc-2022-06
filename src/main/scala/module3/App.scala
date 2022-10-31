package module3

import module3.zio_homework.config
import module3.zio_homework.config.AppConfig
import zio.console.{Console, putStrLn}
import zio.test.mock.MockConsole.PutStrLn
import zio.{ExitCode, URIO, ZIO}

object App {
  def main(args: Array[String]): Unit = {

    zio.Runtime.default.unsafeRun(zioRecursion.fibZ(12).map(_.toString).flatMap(putStrLn(_)))

    // toyModel.echo.run()
  }
}

object ZioApp extends zio.App{
  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] =
    zioDS.ref.updateCounterRef.flatMap(i => zio.console.putStrLn(i.toString))
      .exitCode
}