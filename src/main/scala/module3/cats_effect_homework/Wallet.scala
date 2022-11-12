package module3.cats_effect_homework

import cats.effect.Sync
import cats.implicits._
import module3.cats_effect_homework.Wallet._

import java.nio.file.{Files, Path, Paths}

// DSL управления электронным кошельком
trait Wallet[F[_]] {
  // возвращает текущий баланс
  def balance: F[BigDecimal]
  // пополняет баланс на указанную сумму
  def topup(amount: BigDecimal): F[Unit]
  // списывает указанную сумму с баланса (ошибка если средств недостаточно)
  def withdraw(amount: BigDecimal): F[Either[WalletError, Unit]]
}

// Игрушечный кошелек который сохраняет свой баланс в файл
// todo: реализовать используя java.nio.file._
// Насчёт безопасного конкуррентного доступа и производительности не заморачиваемся, делаем максимально простую рабочую имплементацию. (Подсказка - можно читать и сохранять файл на каждую операцию).
// Важно аккуратно и правильно завернуть в IO все возможные побочные эффекты.
//
// функции которые пригодятся:
// - java.nio.file.Files.write
// - java.nio.file.Files.readString
// - java.nio.file.Files.exists
// - java.nio.file.Paths.get
final class FileWallet[F[_]: Sync](id: WalletId)(path: Path) extends Wallet[F] {
  def balance: F[BigDecimal] = Sync[F].delay(Files.readString(path))
    .map(BigDecimal(_))

  def topup(amount: BigDecimal): F[Unit] = for {
    currentBalance <- balance
    newBalance <- Sync[F].pure(currentBalance + amount)
    _ <- Sync[F].delay(Files.writeString(path, newBalance.toString))
  } yield ()

  def withdraw(amount: BigDecimal): F[Either[WalletError, Unit]] = for {
    currentBalance <- balance
    newBalance <- Sync[F].pure(currentBalance - amount)
    result <- newBalance match {
      case v if v < 0 => Sync[F].pure(BalanceTooLow).map(Left(_))
      case v => Sync[F].delay[Unit](Files.writeString(path, v.toString)).map(Right(_))
    }
  } yield result
}

object Wallet {

  // todo: реализовать конструктор
  // внимание на сигнатуру результата - инициализация кошелька имеет сайд-эффекты
  // Здесь нужно использовать обобщенную версию уже пройденного вами метода IO.delay,
  // вызывается она так: Sync[F].delay(...)
  // Тайпкласс Sync из cats-effect описывает возможность заворачивания сайд-эффектов

  /** В случае если несколько раз использовать один и тот-же id, он перзистентен и не будет перезаписан */
  def fileWallet[F[_]: Sync](id: WalletId): F[Wallet[F]] = for {
    path <- Sync[F].delay(Paths.get(s"$id.wallet"))
    exists <- Sync[F].delay(Files.exists(path))
    _ <- if (!exists) Sync[F].delay(Files.writeString(path, 0.toString))
      else Sync[F].unit
  } yield new FileWallet[F](id)(path)

  type WalletId = String

  sealed trait WalletError
  case object BalanceTooLow extends WalletError
}
