package example

import scalaz._
import Scalaz._
import scalaj.http._
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scalaz.OptionT._
import scalaz.EitherT._

object Services {
  val portfolios = Map("1" -> Map("AAPL" -> 10, "GOOG" -> 100),
    "2" -> Map("MSFT" -> 2000, "GOOG" -> 100))

  val prices = Map("AAPL" -> "788.33", "GOOG" -> "915.3", "MSFT" -> "xxx")

  val Delay = 1

  def getPortfolio(id: String): Future[Option[Map[String, Int]]] = {
    Future.successful {
      Thread.sleep(Delay)
      portfolios.get(id)
    }
  }

  def getPrice(id: String): Future[Option[Double]] = {
    Future {
      Thread.sleep(Delay)
      // Ugly, will get better
      try {
        prices.get(id).map(_.toDouble)
      } catch {
        case _: Exception => None
      }
    }
  }

  def calculatePortfolioValue(m: Map[String, Int]): Future[Option[Double]] = {
    m.map {
      case (k, v) => getPrice(k).map(p => p.map(_ * v))
    }.toList.sequence.map {
      _.fold(Some(0.0)) { case (acc, el) =>
        el.flatMap(value => acc.map(ac => ac + value))
      }
    }
  }

}

// Version 1 - Plain Scala

object Version1 {

  import Services._

  def main(args: Array[String]): Unit = {
    println(Await.result(getPortfolioValue("1"), 5 second))
    println(Await.result(getPortfolioValue("2"), 5 second))
  }


  def getPortfolioValue(id: String) = {
    for {
      p: Option[Map[String, Int]] <- getPortfolio(id)
      v <- p match {
        case Some(m: Map[String, Int]) => calculatePortfolioValue(m)
        case None => Future.successful(None)
      }
    } yield v
  }


}

// Version 2 - Scalaz, Either

object Version2 {

  import Services._

  def main(args: Array[String]): Unit = {
    println(Await.result(getPortfolioValue("1").run, 5 second))
    println(Await.result(getPortfolioValue("2").run, 5 second))
  }

  type ErrorOrValue[A] = EitherT[Future, String, A]

  object ErrorOrValue {

    implicit class FutureOptErrorOrValue[A](fo: Future[Option[A]]) {

      def toErrorOrValue(error: String): ErrorOrValue[A] = {
        val v: Future[\/[String, A]] = fo.map(opt => opt match {
          case Some(a) => \/.right(a)
          case None => \/.left(error)
        })
        eitherT(v)
      }

      implicit class FutureErroring[A](f: Future[A]) {
        def toErrorOrValue: ErrorOrValue[A] = EitherT.right(f)
      }

      def lift[A](value: A): ErrorOrValue[A] = EitherT.right(Future.successful(value))


    }

  }

  import ErrorOrValue._

  def getPortfolioValue(id: String) = {
    for {
      m: Map[String, Int] <- getPortfolio(id) toErrorOrValue ("no options")
      v = calculatePortfolioValue(m)
    } yield v
  }

  // Version 3 - Typeclass

  object Version3 {

    import Services._


    sealed trait ErrorOrResult[F[_], E, A] {
      def flatMap[B](fa: A => ErrorOrResult[F, E, B]): ErrorOrResult[F, E, B]
      def map[B](f: A => B): ErrorOrResult[F, E, B]
    }

    case class Error[F, E, A](e: E) extends ErrorOrResult[F, E, A] {
      override def flatMap[B](fa: A => ErrorOrResult[F, E, B]): ErrorOrResult[F, E, B] = Error[F,E,B](e)
      override def map[B](f: A => B): ErrorOrResult[F, E, B] = Error[F,E,B](e)
    }

   case class Success[A](a: A) extends ErrorOrResult[_, _, A] {
      override def flatMap[B](fa: (A) => ErrorOrResult[_, _, B]): ErrorOrResult[_, _, B] = fa(a)
      override def map[B](f: (A) => B): ErrorOrResult[_, _, B] = Success[B](f(a))
    }

    case class FutureErrorOrResult[E,A](v: Future[E \/ A]) extends ErrorOrResult[Future, E, A] {
      override def flatMap[B](fa: (A) => ErrorOrResult[Future, E, B]): ErrorOrResult[Future, E, B] =
        v.map(v => v match {
          case -\/(e) => Error[Future,E,B](e)
          case \/-(b) => fa(b)
        })


      def map[B](f: A => B): ErrorOrResult[Future, String, B] = {
        val fae = v.map(v => v match {
          case -\/(a) => -\/(a)
          case \/-(b) => \/-(f(b))
        })
        FutureErrorOrResult(fae)
      }
    }

    object FutureErrorOrResult {

      implicit class FutureOptErrorOrResult[A](fo: Future[Option[A]]) {

        def toErrorOrResult[A](error: String): ErrorOrResult[Future, String, A] = {
          val v: Future[\/[String, A]] = fo.map(opt => opt match {
            case Some(a) => \/.right(a)
            case None => \/.left(error)
          })
          FutureErrorOrResult(v)
        }

        implicit class FutureErroring[A](f: Future[A]) {
          def toErrorOrValue: ErrorOrValue[A] = EitherT.right(f)
        }

        def lift[A](value: A): ErrorOrValue[A] = EitherT.right(Future.successful(value))


      }

    }

    import FutureErrorOrResult._

    def getPortfolioValue(id: String) = {
      for {
        m: Map[String, Int] <- getPortfolio(id) toErrorOrResult ("no options")
        v = calculatePortfolioValue(m)
      } yield v
    }

    def main(args: Array[String]): Unit = {
      println(Await.result(getPortfolioValue("1").run, 5 second))
      println(Await.result(getPortfolioValue("2").run, 5 second))
    }
  }

}


