package org.avalyugin.louvre

import akka.actor.{Actor, Props}

case class Account(id: String, balance: Int) {
  def deposit(amount: Int): Account = Account(id, balance + amount)
  def withdraw(amount: Int): Either[String, Account] =
    if (amount > balance) Left("The balance can't be negative after withdraw")
    else Right(Account(id, balance - amount))
}

trait AccountService {

  private var accounts: Map[String, Account] = Map()

  def openAccount(amount: Int): Account = {
    val uuid = java.util.UUID.randomUUID.toString
    val newAccount = Account(uuid, amount)
    accounts += newAccount
    newAccount
  }

  def getAccount(id: String): Either[String, Account] = accounts.get(id) match {
    case Some(account) => Right(account)
    case None => Left(s"No account '${id}' found")
  }

  def transfer(src: Account, dest: Account, amount: Int): Either[String, (Account, Account)] = {
    val result: Either[String, (Account, Account)] = for {
      x <- src.withdraw(amount).right
    } yield (x, dest.deposit(amount))
    result match {
      case Right((x, y)) => List(x, y).foreach(accounts += _)
      case _ =>
    }
    result
  }

}

class AccountServiceActor extends Actor with AccountService {

  import AccountServiceActor._

  override def receive: Receive = {
    case OpenAccount(amount) => sender() ! openAccount(amount)
    case GetAccount(id) => sender() ! getAccount(id)
    case Transfer(src, dest, amount) => sender() ! transfer(src, dest, amount)
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
