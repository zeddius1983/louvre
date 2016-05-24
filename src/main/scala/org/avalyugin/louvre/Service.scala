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
import scala.util.{Failure, Success}

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
      val account = (accountService ? GetAccount(id)).mapTo[Account]
      onComplete(account) {
        case Success(acc) => complete(acc)
        case Failure(error) => complete((StatusCodes.NotFound, error.getMessage))
      }
    }
  }

  val openAccountHandler = post {
    path("accounts" / "open") {
      parameter("amount".as[Int]) { amount =>
        val account = (accountService ? OpenAccount(amount)).mapTo[Account]
        onComplete(account) {
          case Success(acc) => complete((StatusCodes.Created, acc))
          case Failure(error) => complete((StatusCodes.NotAcceptable, error.getMessage))
        }
      }
    }
  }

  val transferHandler = post {
    path("accounts" / Segment / "transfers" / Segment / "new") { (sourceId, destId) =>
      parameter("amount".as[Int]) { amount =>
        val srcRequested = (accountService ? GetAccount(sourceId))
        val destRequested = (accountService ? GetAccount(destId))
        val transfer = for {
          x <- srcRequested.mapTo[Account]
          y <- destRequested.mapTo[Account]
        } yield Transfer(x, y, amount)
        onComplete(transfer) {
          case Success(msg) =>
            accountService ! msg
            complete((StatusCodes.Accepted, s"Requested transfer of ${amount} from '${sourceId}' to '${destId}'"))
          case Failure(error) => complete((StatusCodes.NotFound, error.getMessage))
        }
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


