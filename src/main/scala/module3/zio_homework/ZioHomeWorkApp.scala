package module3.zio_homework

import zio.clock.{Clock, currentDateTime, currentTime}
import zio.config.ReadError
import zio.console.{Console, putStrLn}
import zio.random.Random
import zio.{ExitCode, IO, URIO, ZIO, ZManaged, ZRef}

import java.time.{DateTimeException, OffsetDateTime}
import java.util.concurrent.TimeUnit

object ZioHomeWorkApp extends zio.App {
  override def run(args: List[String]): URIO[Clock with Random with Console, ExitCode] = {
    val guess = guessProgram.exitCode

    val finish: OffsetDateTime = OffsetDateTime.now().plusSeconds(1)
    val whileTest: URIO[Console with Clock, ExitCode] = doWhile(currentDateTime <* putStrLn("Waiting..."))(time => finish.isBefore(time))
      .flatMap(d => putStrLn(s"Waited 1 seconds! Current time: $d")).exitCode

    val configTest: ZManaged[Any, ReadError[String], config.AppConfig] = loadConfigOrDefault
    configTest.useNow.flatMap(cfg => putStrLn(cfg.toString)).exitCode
  }
}
