package module3

import module3.toyModel.F
import zio.{Has, Schedule, Task, ULayer, ZIO, ZLayer, ZManaged}
import zio.clock.{Clock, sleep}
import zio.config.ReadError
import zio.console._
import zio.duration.durationInt
import zio.macros.accessible
import zio.random._

import java.io.IOException
import java.lang.ref.PhantomReference
import java.util.concurrent.TimeUnit
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
  lazy val eff = ???

  /**
   * 4.2 Создайте коллукцию из 10 выше описанных эффектов (eff)
   */
  lazy val effects = ???

  
  /**
   * 4.3 Напишите программу которая вычислит сумму элементов коллекци "effects",
   * напечатает ее в консоль и вернет результат, а также залогирует затраченное время на выполнение,
   * можно использовать ф-цию printEffectRunningTime, которую мы разработали на занятиях
   */

  lazy val app = ???


  /**
   * 4.4 Усовершенствуйте программу 4.3 так, чтобы минимизировать время ее выполнения
   */

  lazy val appSpeedUp = ???


  /**
   * 5. Оформите ф-цию printEffectRunningTime разработанную на занятиях в отдельный сервис, так чтобы ее
   * молжно было использовать аналогично zio.console.putStrLn например
   */


   /**
     * 6.
     * Воспользуйтесь написанным сервисом, чтобы созадть эффект, который будет логировать время выполнения прогаммы из пункта 4.3
     *
     * 
     */

  lazy val appWithTimeLogg = ???

  /**
    * 
    * Подготовьте его к запуску и затем запустите воспользовавшись ZioHomeWorkApp
    */

  lazy val runApp = ???
  
}
