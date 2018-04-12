package services

import connectors.MathsConnector
import exceptions.ThatsNegativeException

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object MathsService {

  def addTwoNumbers(a: Int, b: Int): Future[Int] = MathsConnector.add(a, b)

  def subtractTwoNumbers(a: Int, b: Int): Future[Int] = MathsConnector.subtract(a, b)

  def subtractToString(a: Int, b: Int): Future[String] = {
    MathsConnector.subtract(a, b).map {
      _.toString
    }
  }

  def addIfResultIsEven(a: Int, b: Int): Future[Int] = {
    MathsConnector.add(a, b) map {
      case num if num % 2 == 0 => num
      case _ => a
    }
  }

  def addThenSubtract(startingNumber: Int, numToAdd: Int, numToSubtract: Int): Future[Int] = {

    //EITHER
    val method1 = for {
      a <- MathsConnector.add(startingNumber, numToAdd)
      res <- MathsConnector.subtract(a, numToSubtract)
    } yield res

    //OR
    val method2 = MathsConnector.add(startingNumber, numToAdd) flatMap {
      a => MathsConnector.subtract(a, numToSubtract)
    }

    method1
    // method2
  }

  def subtractErrorIfNegative(a: Int, b: Int): Future[Int] = MathsConnector.subtractErrorIfNegative(a, b)

  def subtractBut1000ifNegative(a: Int, b: Int): Future[Int] = {
    MathsConnector.subtractErrorIfNegative(a, b).recover {
      case _: ThatsNegativeException => 1000
    }
  }

  def divide0IfError(a: Int, b: Int): Future[Int] = {
    MathsConnector.divide(a, b).recover {
      case _: ArithmeticException => 0
    }
  }

  def divideAndThenSubtractWithErrorDefaults(startingNumber: Int, numToDivideBy: Int, numToSubtract: Int): Future[Int] = {
    {
      for {
        a <- MathsConnector.divide(startingNumber, numToDivideBy)
        res <- MathsConnector.subtractErrorIfNegative(a, numToSubtract)
      } yield res
    }.recover {
      case _: ThatsNegativeException => 1000
      case _: ArithmeticException => 0
    }
  }

}
