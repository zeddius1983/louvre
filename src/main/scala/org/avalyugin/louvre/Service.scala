package org.avalyugin.louvre

import akka.actor.{ActorRef, ActorSystem}
import akka.event.{Logging, LoggingAdapter}
import akka.http.scaladsl.Http
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import akka.pattern.ask
import akka.stream.{ActorMaterializer, Materializer}
import akka.util.Timeout
import com.typesafe.config.{Config, ConfigFactory}
import org.avalyugin.louvre.AccountServiceActor.{GetAccount, OpenAccount, Transfer}
import spray.json.DefaultJsonProtocol._

import scala.concurrent.ExecutionContextExecutor
import scala.concurrent.duration._
import scala.io.StdIn

trait AccountServiceRoute {

  implicit val system: ActorSystem

  implicit def executor: ExecutionContextExecutor

  implicit val materializer: Materializer

  def config: Config

  val logger: LoggingAdapter

  val accountService: ActorRef

  implicit val timeout: Timeout = 5.seconds
  implicit val accountFormat = jsonFormat2(Account.apply)

  val getAccountHandler = get {
    path("accounts" / Segment) { id =>
      val account = (accountService ? GetAccount(id)).mapTo[Either[String, Account]]
      onSuccess(account) {
        case Left(msg) => complete((StatusCodes.NotFound, msg))
        case Right(acc) => complete(acc)
      }
    }
  }

  val openAccountHandler = post {
    parameter("amount".as[Int]) { amount =>
      path("accounts" / "open") {
        val account = (accountService ? OpenAccount(amount)).mapTo[Account]
        complete((StatusCodes.Created, account))
      }
    }
  }

  val transferHandler = post {
    parameter("dest", "amount".as[Int]) { (destId, amount) =>
      path("accounts" / Segment / "transfers" / "new") { sourceId =>
        val srcAcc = (accountService ? GetAccount(sourceId)).mapTo[Either[String, Account]]
        val destAcc = (accountService ? GetAccount(destId)).mapTo[Either[String, Account]]
        // TODO: it's too late...
        complete((StatusCodes.Accepted, s"Requested transfer of ${amount} from '${sourceId}' to '${destId}'"))
      }
    }
  }

  val route = getAccountHandler ~ openAccountHandler ~ transferHandler

}

object Server extends App with AccountServiceRoute {
  override implicit val system = ActorSystem()
  override implicit val executor = system.dispatcher
  override implicit val materializer = ActorMaterializer()

  override val config = ConfigFactory.load()
  override val logger = Logging(system, getClass)
  override val accountService = system.actorOf(AccountServiceActor.props(), "AccountServiceActor")

  val bindingFuture = Http().bindAndHandle(route, "localhost", 8080)

  println(s"Server online at http://localhost:8080/\nPress RETURN to stop...")
  StdIn.readLine() // let it run until user presses return
  bindingFuture
    .flatMap(_.unbind()) // trigger unbinding from the port
    .onComplete(_ ⇒ system.terminate()) // and shutdown when done
}


