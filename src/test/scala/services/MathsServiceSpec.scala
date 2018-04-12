package services

import exceptions.ThatsNegativeException
import org.scalatest.{Matchers, WordSpec}
import testutils.PromiseResolvers

class MathsServiceSpec extends WordSpec with Matchers with PromiseResolvers {

  "addTwoNumbers" should {
    "add 3 and 4" in {
      await(MathsService.addTwoNumbers(3, 4)) shouldBe 7
    }
    "add 0 and 300" in {
      await(MathsService.addTwoNumbers(0, 300)) shouldBe 300
    }
    "add -12 and -29" in {
      await(MathsService.addTwoNumbers(-12, -29)) shouldBe -41
    }
  }

  "subtractTwoNumbers" should {
    "subtract 10 and 4" in {
      await(MathsService.subtractTwoNumbers(10, 4)) shouldBe 6
    }
    "subtract 52 and 0" in {
      await(MathsService.subtractTwoNumbers(52, 0)) shouldBe 52
    }
    "subtract -15 and -246" in {
      await(MathsService.subtractTwoNumbers(-15, -246)) shouldBe 231
    }
  }

  "subtractToString" should {
    "subtract 40 and 30" in {
      await(MathsService.subtractToString(40, 30)) shouldBe "10"
    }
    "subtract 4 and 89" in {
      await(MathsService.subtractToString(4, 89)) shouldBe "-85"
    }
    "subtract 0 and -3" in {
      await(MathsService.subtractToString(0, -3)) shouldBe "3"
    }
  }

  "addIfResultIsEven" should {
    "return the first number unchanged" when {
      "adding 3 and 4" in {
        await(MathsService.addIfResultIsEven(3, 4)) shouldBe 3
      }
      "adding 10 and 10001" in {
        await(MathsService.addIfResultIsEven(10, 10001)) shouldBe 10
      }
    }
    "add two numbers" when {
      "adding 10 and 12" in {
        await(MathsService.addIfResultIsEven(10, 12)) shouldBe 22
      }
      "adding 3 and 7" in {
        await(MathsService.addIfResultIsEven(3, 7)) shouldBe 10
      }
      "adding 9 and -27" in {
        await(MathsService.addIfResultIsEven(9, -27)) shouldBe -18
      }
    }
  }

  "addThenSubtract" should {
    "add 3 and take 4 from 5" in {
      await(MathsService.addThenSubtract(5, 3, 4)) shouldBe 4
    }
    "add 12 and take 2 from 0" in {
      await(MathsService.addThenSubtract(0, 12, 2)) shouldBe 10
    }
    "add -20 and take 13 from -100" in {
      await(MathsService.addThenSubtract(-100, -20, 13)) shouldBe -133
    }
  }

  "subtractErrorIfNegative" should {
    "subtract 1 from 10" in {
      await(MathsService.subtractErrorIfNegative(10, 1)) shouldBe 9
    }
    "subtract 0 from 3" in {
      await(MathsService.subtractErrorIfNegative(3, 0)) shouldBe 3
    }
    "subtract 200 from 200" in {
      await(MathsService.subtractErrorIfNegative(200, 200)) shouldBe 0
    }
    "throw a negative exception if the resulting number is less than zero" in {
      a[ThatsNegativeException] shouldBe thrownBy (await(MathsService.subtractErrorIfNegative(10, 20)))
    }
  }

  "subtractBut1000ifNegative()" should {
    "subtract 1 from 10" in {
      await(MathsService.subtractBut1000ifNegative(10, 1)) shouldBe 9
    }
    "subtract 0 from 3" in {
      await(MathsService.subtractBut1000ifNegative(3, 0)) shouldBe 3
    }
    "subtract 200 from 200" in {
      await(MathsService.subtractBut1000ifNegative(200, 200)) shouldBe 0
    }
    "return 1000 if the resulting number is less than zero" in {
      await(MathsService.subtractBut1000ifNegative(10, 20)) shouldBe 1000
    }
  }

  "divide0IfError" should {
    "divide 10 by 1" in {
      await(MathsService.divide0IfError(10, 1)) shouldBe 10
    }
    "divide 96 by 4" in {
      await(MathsService.divide0IfError(96, 4)) shouldBe 24
    }
    "divide 200 by 200" in {
      await(MathsService.divide0IfError(200, 200)) shouldBe 1
    }
    "return 0 when dividing by 0" in {
      await(MathsService.divide0IfError(10, 0)) shouldBe 0
    }
  }

  "divideAndThenSubtractWithErrorDefaults" should {
    "divide 10 by 2 then subtract 3" in {
      await(MathsService.divideAndThenSubtractWithErrorDefaults(10, 2, 3)) shouldBe 2
    }
    "divide 48 by 6 then subtract -10" in {
      await(MathsService.divideAndThenSubtractWithErrorDefaults(48, 6, -10)) shouldBe 18
    }
    "return 0 when dividing by zero" in {
      await(MathsService.divideAndThenSubtractWithErrorDefaults(10, 0, 1)) shouldBe 0
    }
    "return 1000 when the resulting number is less than zero" in {
      await(MathsService.divideAndThenSubtractWithErrorDefaults(10, 5, 20)) shouldBe 1000
    }
  }

}
