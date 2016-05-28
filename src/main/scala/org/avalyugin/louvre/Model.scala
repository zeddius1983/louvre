package org.avalyugin.louvre

import akka.actor.{Actor, Props}

case class AccountException(msg: String) extends Exception(msg)

case class Account(id: String, balance: Int) {
  def deposit(amount: Int): Account = Account(id, balance + amount)
  def withdraw(amount: Int): Account =
    if (amount > balance) throw AccountException(s"The account [$id] balance can't be negative after withdrawal")
    else Account(id, balance - amount)
}

trait AccountService {

  final val MaxBalance: Int = 1000

  final val OpenAccountBalance: Int = 0

  var accounts: Map[String, Account]

  def openAccount(amount: Option[Int]): Account = {
    val initialBalance = amount.getOrElse(OpenAccountBalance)
    if (initialBalance > MaxBalance) throw AccountException(s"Can't open accounts with initial balance > ${MaxBalance}")
    val uuid = java.util.UUID.randomUUID.toString
    val newAccount = Account(uuid, initialBalance)
    accounts += newAccount
    newAccount
  }

  def getAccount(id: String): Account = accounts.get(id) match {
    case Some(account) => account
    case None => throw AccountException(s"No account [$id] found")
  }

  def transfer(src: Account, dest: Account, amount: Int): (Account, Account) = {
    val newSrc = src.withdraw(amount)
    val newDest = dest.deposit(amount)
    accounts += newSrc
    accounts += newDest
    (newSrc, newDest)
  }

}

class AccountServiceActor(override var accounts: Map[String, Account] = Map()) extends Actor with AccountService {

  import AccountServiceActor._

  override def receive: Receive = handleExceptions {
    case OpenAccount(amount) => sender() ! openAccount(amount)
    case GetAccount(id) => sender() ! getAccount(id)
  } orElse {
    // Do not handle exceptions for transfer because we're not sending response back to sender
    case Transfer(src, dest, amount) => transfer(src, dest, amount)
  }

  /**
    * Wraps the partial function to handle exceptions and send back Failure status.
    */
  def handleExceptions(f: Receive): Receive = {
    case msg if f.isDefinedAt(msg) =>
      try {
        f.apply(msg)
      } catch {
        case e: Exception =>
          sender() ! akka.actor.Status.Failure(e)
          throw e
      }
  }

  // TODO: which of two is better???
  /** def handleExceptions2(f: Receive): Receive = new Receive {
    override def isDefinedAt(msg: Any): Boolean = f.isDefinedAt(msg)
    override def apply(msg: Any): Unit =
      try {
        f.apply(msg)
      } catch {
        case e: Exception =>
          sender() ! akka.actor.Status.Failure(e)
          throw e
      }
  }**/

}

object Account {

  def apply(id: String): Account = new Account(id, 0)

  import scala.language.implicitConversions
  /**
    * Implicitely converts Account to map entry.
    */
  implicit def accountToMap(account: Account): (String, Account) = (account.id, account)

}

object AccountServiceActor {

  case class GetAccount(id: String)

  case class OpenAccount(amount: Option[Int])

  case class Transfer(src: Account, dest: Account, amount: Int)

  def props(): Props = Props(new AccountServiceActor())

  def props(accounts: Map[String, Account]): Props = Props(new AccountServiceActor(accounts))
}
