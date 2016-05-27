package org.avalyugin.louvre

import akka.event.Logging
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.testkit.ScalatestRouteTest
import org.scalatest.{Matchers, WordSpec}

class ServiceTest extends WordSpec with Matchers with ScalatestRouteTest with AccountServiceRoute {

  val testAccounts = Map(Account("test1", 40))

  override val logger = Logging(system, getClass)
  override val accountService = system.actorOf(AccountServiceActor.props(testAccounts), "AccountServiceActor")

  "The service" should {
    "return an Account for GET requests to the path" in {
      Get("/accounts/test1") ~> route ~> check {
        responseAs[Account] shouldEqual Account("test1", 40)
      }
    }
  }

  "The service" should {
    "return 404 if Account is missing for GET requests to the path" in {
      Get("/accounts/test2") ~> route ~> check {
        status shouldEqual StatusCodes.NotFound
        responseAs[String] shouldEqual "No account 'test2' found"
      }
    }
  }

}
