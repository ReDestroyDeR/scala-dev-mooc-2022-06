package module3

import module3.toyModel.F
import module3.zioConcurrency.printEffectRunningTime
import module3.zio_homework.runningTimeTracker.RunningTimeTracker
import zio.{ExitCode, Has, Schedule, Task, ULayer, URIO, URLayer, ZIO, ZLayer, ZManaged, ZRef}
import zio.clock.{Clock, currentDateTime, currentTime, sleep}
import zio.config.ReadError
import zio.console._
import zio.duration.durationInt
import zio.macros.accessible
import zio.random._

import java.io.IOException
import java.lang.ref.PhantomReference
import java.util.concurrent.TimeUnit
import java.util.concurrent.TimeUnit.SECONDS
import scala.::
import scala.collection.immutable.HashSet
import scala.collection.{BuildFrom, mutable}
import scala.io.StdIn
import scala.language.postfixOps
import scala.ref.WeakReference

package object zio_homework {
  /**
   * 1.
   * Используя сервисы Random и Console, напишите консольную ZIO программу которая будет предлагать пользователю угадать число от 1 до 3
   * и печатать в когнсоль угадал или нет. Подумайте, на какие наиболее простые эффекты ее можно декомпозировать.
   */



  lazy val guessProgram = for {
    lowerBound <- ZIO.succeed(1)
    upperBound <- ZIO.succeed(3)
    secret <- nextIntBetween(lowerBound, upperBound)
    prompt = () => putStrLn(s"Угадайте число от $lowerBound до $upperBound!")
    _ <- prompt()
    // Не уверен как декомпозировать валидацию не прибегая к определению своих типов данных
    // Не ложиться немножко функциональная идея :(
    number <- getStrLn.flatMap(input => ZIO.effect(input.toInt)
                      .mapError{
                        case error: NumberFormatException => s"\"$input\" - не является числом! Введите число"
                        case error => s"Неизвестная ошибка для входных данных \"$input\"\n${error.toString}"
                      }
                      .filterOrFail(in => lowerBound <= in &&  in <= upperBound)
                                  (s"Число не в промежутке от $lowerBound до $upperBound!")
    ).onError(error => putStrLn(error.failures.mkString("\n")) *> prompt())
      .retry(Schedule.forever)
    endGame <- ZIO.effect(number == secret).map {
      case true => s"Ура! Вы угадали! Число $secret"
      case false => s"О нет... Вы не угадали( Число было $secret"
    }
    _ <- putStrLn(endGame)
  } yield ()

  /**
   * 2. реализовать функцию doWhile (общего назначения), которая будет выполнять эффект до тех пор, пока его значение в условии не даст true
   * 
   */

  def doWhile[R, E, A](effect: ZIO[R, E, A])(predicate: A => Boolean): ZIO[R, E, A] =
    effect.flatMap(self => if (predicate(self)) {
      ZIO.succeed(self)
    } else {
      doWhile(effect)(predicate)
    })

  def doWhileM[R, E, A, R1, E1](effect: ZIO[R, E, A])(predicate: A => ZIO[R1, E1, Boolean]): ZIO[R with R1, Any, A] =
    effect.flatMap(self => predicate(self).flatMap(res => if (res) {
      ZIO.succeed(self)
    } else {
      doWhileM(effect)(predicate)
    }))

  /**
   * 3. Реализовать метод, который безопасно прочитает конфиг из файла, а в случае ошибки вернет дефолтный конфиг
   * и выведет его в консоль
   * Используйте эффект "load" из пакета config
   */


  def loadConfigOrDefault: ZManaged[Any, ReadError[String], config.AppConfig] = ZManaged.fromEffect(config.load)
    .orElseSucceed(config.AppConfig("localhost", "8080"))


  /**
   * 4. Следуйте инструкциям ниже для написания 2-х ZIO программ,
   * обратите внимание на сигнатуры эффектов, которые будут у вас получаться,
   * на изменение этих сигнатур
   */


  /**
   * 4.1 Создайте эффект, который будет возвращать случайеым образом выбранное число от 0 до 10 спустя 1 секунду
   * Используйте сервис zio Random
   */
  lazy val eff: URIO[Random with Clock, Int] = for {
    _ <- sleep(1 second)
    random <- nextIntBetween(0, 10)
  } yield random

  /**
   * 4.2 Создайте коллукцию из 10 выше описанных эффектов (eff)
   */
  lazy val effects: URIO[Random with Clock, Iterable[Int]] = eff.replicateM(10)
  
  /**
   * 4.3 Напишите программу которая вычислит сумму элементов коллекци "effects",
   * напечатает ее в консоль и вернет результат, а также залогирует затраченное время на выполнение,
   * можно использовать ф-цию printEffectRunningTime, которую мы разработали на занятиях
   */

  lazy val app: URIO[Console with Clock with Random, Int] = printEffectRunningTime(for {
    sum <- effects.map(_.sum)
    _ <- putStrLn(s"Sum is $sum")
  } yield sum)


  /**
   * 4.4 Усовершенствуйте программу 4.3 так, чтобы минимизировать время ее выполнения
   *
   * Аргумент в том, что мы захардкодили условие 10 элементов. При изменении условий, 4.2 будет изменен,
   * но 4.4 нет
   */

  lazy val appSpeedUp = for {
    // Можно использовать collectAllPar .map(_.sum), это более verbose вариант
    sum <- ZIO.mergeAllParN(Runtime.getRuntime.availableProcessors())(ZIO.replicate(10)(eff))(0)(_+_)
    _ <- putStrLn(s"Sum is $sum")
  } yield sum


  /**
   * 5. Оформите ф-цию printEffectRunningTime разработанную на занятиях в отдельный сервис, так чтобы ее
   * молжно было использовать аналогично zio.console.putStrLn например
   */
  object runningTimeTracker {
    type RunningTimeTracker = Has[RunningTimeTracker.Service]

    object RunningTimeTracker {
      trait Service {
        def printEffectRunningTime[R, E, A](eff: ZIO[R, E, A]): ZIO[R, E, A]
      }


      // Можем отбросить зависимости и использовать ZLayer.success
      val live: URLayer[Console with Clock, RunningTimeTracker] =
        ZLayer.fromServices[Clock.Service, Console.Service, RunningTimeTracker.Service] {
          (clock: Clock.Service, console: Console.Service) =>
            new Service {

              override def printEffectRunningTime[R, E, A](eff: ZIO[R, E, A]): ZIO[R, E, A] = for {
                start <- clock.currentTime(SECONDS)
                retVal <- eff
                end <- clock.currentTime(SECONDS)
                _ <- console.putStrLn(s"Time elapsed: ${end - start}")
              } yield retVal

            }
        }
    }

    def printEffectRunningTime[R, E, A](eff: ZIO[R, E, A]): ZIO[R with RunningTimeTracker, E, A] =
      ZIO.accessM(_.get.printEffectRunningTime(eff))

  }



   /**
     * 6.
     * Воспользуйтесь написанным сервисом, чтобы созадть эффект, который будет логировать время выполнения прогаммы из пункта 4.3
     *
     * 
     */

  lazy val appWithTimeLogg: ZIO[Console with Random with Clock with RunningTimeTracker, Nothing, Int] = runningTimeTracker.printEffectRunningTime(appSpeedUp)

  /**
    * 
    * Подготовьте его к запуску и затем запустите воспользовавшись ZioHomeWorkApp
    */

  lazy val runApp: ZIO[Console with Clock with Random, Nothing, Int] =
    appWithTimeLogg.provideSomeLayer[Console with Clock with Random](RunningTimeTracker.live)
  
}
