package example

import org.scalatest._

import scala.concurrent.Await
import scala.concurrent.duration._

class ServiceSpec extends FlatSpec with Matchers {
  "The Hello object" should "say hello" in {

    Await.result(Services.getPortfolio("1"), 5 seconds) shouldEqual Some(List("AAPL"))
  }
}
