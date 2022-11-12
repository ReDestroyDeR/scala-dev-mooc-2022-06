package module3.cats_effect_homework

import cats.effect.implicits._
import cats.effect.kernel.Fiber
import cats.effect.{IO, IOApp, Spawn}
import cats.implicits._

import scala.concurrent.duration.{Duration, FiniteDuration, MILLISECONDS, SECONDS}

// Поиграемся с кошельками на файлах и файберами.

// Нужно написать программу где инициализируются три разных кошелька и для каждого из них работает фоновый процесс,
// который регулярно пополняет кошелек на 100 рублей раз в определенный промежуток времени. Промежуток надо сделать разный, чтобы легче было наблюдать разницу.
// Для определенности: первый кошелек пополняем раз в 100ms, второй каждые 500ms и третий каждые 2000ms.
// Помимо этих трёх фоновых процессов (подсказка - это файберы), нужен четвертый, который раз в одну секунду будет выводить балансы всех трех кошельков в консоль.
// Основной процесс программы должен просто ждать ввода пользователя (IO.readline) и завершить программу (включая все фоновые процессы) когда ввод будет получен.
// Итого у нас 5 процессов: 3 фоновых процесса регулярного пополнения кошельков, 1 фоновый процесс регулярного вывода балансов на экран и 1 основной процесс просто ждущий ввода пользователя.

// Можно делать всё на IO, tagless final тут не нужен.

// Подсказка: чтобы сделать бесконечный цикл на IO достаточно сделать рекурсивный вызов через flatMap:
// def loop(): IO[Unit] = IO.println("hello").flatMap(_ => loop())
object WalletFibersApp extends IOApp.Simple {
  def spawnAddProcess(wallet: Wallet[IO], delayBy: FiniteDuration): IO[Fiber[IO, Throwable, Unit]] =
    Spawn[IO].start(
        wallet.topup(100)
          .delayBy(delayBy)
          .foreverM
      )

  def run: IO[Unit] =
    for {
      _ <- IO.println("Press any key to stop...")
      wallet1 <- Wallet.fileWallet[IO]("1")
      wallet2 <- Wallet.fileWallet[IO]("2")
      wallet3 <- Wallet.fileWallet[IO]("3")
      // todo: запустить все файберы и ждать ввода от пользователя чтобы завершить работу
      fibers <- spawnAddProcess(wallet1, Duration(100, MILLISECONDS)) *>
        spawnAddProcess(wallet2, Duration(200, MILLISECONDS)) *>
        spawnAddProcess(wallet3, Duration(500, MILLISECONDS)) *>
        Spawn[IO].start(
          (wallet1, wallet2, wallet3).traverse[IO, Unit](wallet =>
            wallet.balance
                  .flatMap(balance => IO.println(s"${wallet.hashCode()}: $balance"))
          ).delayBy(Duration(1, SECONDS))
           .foreverM
        )
      _ <- IO.readLine *> fibers.cancel
    } yield ()

}
