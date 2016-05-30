package org.avalyugin.louvre

import akka.event.Logging
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.testkit.ScalatestRouteTest
import org.scalatest.{Matchers, WordSpec}

class ServiceTest extends WordSpec with Matchers with ScalatestRouteTest with AccountServiceRoute {

  val testAccounts = Map(Account("odersky", 40), Account("miller", 30))

  override val logger = Logging(system, getClass)
  override val accountService = system.actorOf(AccountServiceActor.props(testAccounts), "AccountServiceActor")

  val route = accountRoute

  "The service" should {
    "return an Account for GET requests to the path" in {
      Get("/accounts/odersky") ~> route ~> check {
        responseAs[Account] shouldEqual Account("odersky", 40)
      }
    }
  }

  "The service" should {
    "return 404 if Account is missing for GET requests to the path" in {
      Get("/accounts/gates") ~> route ~> check {
        status shouldEqual StatusCodes.NotFound
        responseAs[String] shouldEqual "No account [gates] found"
      }
    }
  }

  "The service" should {
    "open new account with certain balance for POST requests to the path" in {
      Post("/accounts/open?amount=100") ~> route ~> check {
        status shouldEqual StatusCodes.Created
        responseAs[Account] should matchPattern { case Account(_, 100) => }
      }
    }
  }

  "The service" should {
    "open new account with default balance for POST requests to the path" in {
      Post("/accounts/open") ~> route ~> check {
        status shouldEqual StatusCodes.Created
        responseAs[Account] should matchPattern { case Account(_, 0) => }
      }
    }
  }

  "The service" should {
    "return BadRequest for POST requests to the path" in {
      Post("/accounts/open?amount=2000") ~> route ~> check {
        status shouldEqual StatusCodes.BadRequest
        responseAs[String] shouldEqual "Can't open accounts with initial balance > 1000"
      }
    }
  }

  "The service" should {
    "return 404 for POST requests to the path" in {
      Post("/accounts/odersky/transfers/gates/new?amount=10") ~> route ~> check {
        status shouldEqual StatusCodes.NotFound
        responseAs[String] shouldEqual "No account [gates] found"
      }
    }
  }

  "The service" should {
    "accept POST requests to the path" in {
      Post("/accounts/odersky/transfers/miller/new?amount=10") ~> route ~> check {
        status shouldEqual StatusCodes.Accepted
      }
    }
  }

}
