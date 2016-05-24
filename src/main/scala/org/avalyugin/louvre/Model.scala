package org.avalyugin.louvre

import akka.actor.{Actor, Props}

import scala.util.Try

case class AccountException(msg: String) extends Exception(msg)

case class Account(id: String, balance: Int) {
  def deposit(amount: Int): Account = Account(id, balance + amount)

  def withdraw(amount: Int): Account =
    if (amount > balance) throw AccountException(s"The account '${id}' balance can't be negative after withdraw")
    else Account(id, balance - amount)
}

trait AccountService {

  private var accounts: Map[String, Account] = Map(
    "test1" -> Account("test1", 50),
    "test2" -> Account("test2", 20)
  )

  def openAccount(amount: Int): Account = {
    val uuid = java.util.UUID.randomUUID.toString
    val newAccount = Account(uuid, amount)
    accounts += newAccount
    newAccount
  }

  def getAccount(id: String): Account = accounts.get(id) match {
    case Some(account) => account
    case None => throw AccountException(s"No account '${id}' found")
  }

  def transfer(src: Account, dest: Account, amount: Int): (Account, Account) = {
    val newSrc = src.withdraw(amount)
    val newDest = dest.deposit(amount)
    accounts += newSrc
    accounts += newDest
    (newSrc, newDest)
  }

}

class AccountServiceActor extends Actor with AccountService {

  import AccountServiceActor._

  override def receive: Receive = handleExceptions {
    case OpenAccount(amount) => sender() ! openAccount(amount)
    case GetAccount(id) => sender() ! getAccount(id)
    case Transfer(src, dest, amount) => Try(transfer(src, dest, amount)) // ignore
  }

  def handleExceptions(f: Receive): Receive = {
    case msg =>
      try {
        f.apply(msg)
      } catch {
        case e: Exception =>
          sender() ! akka.actor.Status.Failure(e)
          throw e
      }
  }


}

object Account {

  def apply(id: String): Account = new Account(id, 0)

  implicit def accountToMap(account: Account): (String, Account) = (account.id, account)

}

object AccountServiceActor {

  case class GetAccount(id: String)

  case class OpenAccount(amount: Int)

  case class Transfer(src: Account, dest: Account, amount: Int)

  def props(): Props = Props(new AccountServiceActor())
}
