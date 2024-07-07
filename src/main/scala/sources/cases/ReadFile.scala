package sources.cases

import cats.effect.kernel.Resource
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits.catsSyntaxTuple2Semigroupal

import java.io._

object ReadFile {

  def inputStream(f: File) =
    Resource.make {
      IO.blocking(new FileInputStream(f))
    } { inStream =>
      IO.blocking(inStream.close()).handleErrorWith(_ => IO.unit)
    }

  def outputStream(f: File) =
    Resource.make(IO.blocking(new FileOutputStream(f))) {
      outputStream =>
        IO.blocking(outputStream.close())
          .handleErrorWith(_ => IO.unit)
    }

  def inputOutputStream(in: File, out: File) =
    for {
      inStream <- inputStream(in)
      outStream <- outputStream(out)
    } yield (inStream, outStream)


  def inputStreamAuto(f: File) =
    Resource.fromAutoCloseable(IO(new FileInputStream(f)))

  def outputStreamAuto(f: File) =
    Resource.fromAutoCloseable(IO(new FileOutputStream(f)))

  def transfer(origin: FileInputStream, destination: FileOutputStream): IO[Long] = {

    def transmit(origin: FileInputStream, destination: FileOutputStream,
                 buffer: Array[Byte], acc: Long): IO[Long] =
      for {
        amount <- IO.blocking(origin.read(buffer, 0, buffer.length))
        count <- if (amount > -1)
          IO.blocking(destination.write(buffer, 0, amount)) >>
            transmit(origin, destination, buffer, acc + amount)
        else IO.pure(acc)
      } yield count

    transmit(origin, destination, new Array[Byte](1024 * 10), 0L)
  }

  def copy(origin: File, dest: File) =
    inputOutputStream(origin, dest).use {
      case (in, out) =>
        transfer(in, out)
    }

  def copy_(origin: File, dest: File) = {
    val in = IO(new FileInputStream(origin))
    val out = IO(new FileOutputStream(origin))
    (in, out)
      .tupled
      .bracket {
        case (i, o) =>
          transfer(i, o)
      } {
        case (i, o) =>
          (IO(i.close()), IO(o.close()))
            .tupled
            .void.handleErrorWith(_ => IO.unit)
      }
  }

  import cats.effect.Sync
  import cats.syntax.all._

  def transmitF[F[_] : Sync](origin: InputStream, destination: OutputStream, buffer: Array[Byte], acc: Long): F[Long] =
    for {
      amount <- Sync[F].blocking(origin.read(buffer, 0, buffer.length))
      count <- if (amount > -1) Sync[F].blocking(destination.write(buffer, 0, amount)) >> transmitF(origin, destination, buffer, acc + amount)
      else Sync[F].pure(acc)
    } yield count


  def transferF[F[_] : Sync](origin: InputStream, destination: OutputStream): F[Long] =
    transmitF(origin, destination, new Array[Byte](1024 * 10), 0L)

  def inputStreamF[F[_] : Sync](f: File): Resource[F, FileInputStream] =
    Resource.make(Sync[F].blocking(new FileInputStream(f))) {
      inStream =>
        Sync[F].blocking(inStream.close()).handleErrorWith(error => {
          println(s" origin file error: ${error}")
          Sync[F].unit
        })
    }

  def outputStreamF[F[_] : Sync](f: File): Resource[F, FileOutputStream] =
    Resource.make(Sync[F].blocking(new FileOutputStream(f))) {
      outStream =>
        Sync[F].blocking(outStream.close()).handleErrorWith(error => {
          println(s" dest file error: ${error}")
          Sync[F].unit
        })
    }

  def pathCheckF[F[_] : Sync](in: String, out: String) =
    Resource.make(Sync[F].blocking(in === out)) {
      pathEq =>
        if (pathEq) throw new IllegalStateException("the in path eqs the out path")
        Sync[F].unit
    }.use(a => Sync[F].pure(a))

  def inputOutputStreamsF[F[_] : Sync](in: File, out: File): Resource[F, (InputStream, OutputStream)] =
    for {
      inStream <- inputStreamF(in)
      outStream <- outputStreamF(out)
    } yield (inStream, outStream)

  def copyF[F[_] : Sync](origin: File, destination: File): F[Long] =
    inputOutputStreamsF(origin, destination).use {
      case (in, out) =>
        transferF(in, out)
    }

}

object ResourceCase extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = {
    println(s"$args")
    for {
      _ <- if (args.length < 2)
        IO.raiseError(new IllegalArgumentException("参数错误"))
      else
        IO.unit
      orig = new File(args.head)
      dest = new File(args(1))
      count <- ReadFile.copy(orig, dest)
      _ <- IO.println(s"$count bytes copied from ${orig.getPath} to ${dest.getPath}")
    } yield ExitCode.Success
  }

}

object ResourceCaseF extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = {
    println(s"$args")
    for {
      _ <- if (args.length < 2)
        IO.raiseError(new IllegalArgumentException("参数错误"))
      else
        IO.unit
      orig = new File(args.head)
      dest = new File(args(1))
      _ <- ReadFile.pathCheckF[IO](args.head, args(1))
      count <- ReadFile.copyF[IO](orig, dest)
      _ <- IO.println(s"$count bytes copied from ${orig.getPath} to ${dest.getPath}")
    } yield ExitCode.Success
  }

}
































