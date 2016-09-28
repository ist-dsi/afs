package pt.tecnico.dsi.afs

import com.typesafe.scalalogging.LazyLogging

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration.DurationInt
import org.scalatest._
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.exceptions.TestFailedException
import work.martins.simon.expect.core.Expect

trait TestUtils extends ScalaFutures with Matchers with EitherValues with LazyLogging { self: AsyncTestSuite ⇒
  implicit class RichExpect[T](expect: Expect[Either[ErrorCase, T]]) {
    def test(test: Either[ErrorCase, T] ⇒ Assertion): Future[Assertion] = expect.run().map(test)

    def rightValue(testOnRight: T => Assertion): Future[Assertion] = test(t => testOnRight(t.right.get))
    def leftValue(testOnLeft: ErrorCase => Assertion): Future[Assertion] = test(t => testOnLeft(t.left.get))

    def rightValueShouldBe(t: T): Future[Assertion] = rightValue(_ shouldBe t)
    def rightValueShouldBeUnit()(implicit ev: T =:= Unit): Future[Assertion] = rightValue(_.shouldBe(()))
    def leftValueShouldBe(error: ErrorCase): Future[Assertion] = leftValue(_ shouldBe error)

    def idempotentTest(test: Either[ErrorCase, T] => Assertion, repetitions: Int = 3): Future[Assertion] = {
      require(repetitions >= 2, "To test for idempotency at least 2 repetitions must be made")

      expect.run().flatMap { firstResult ⇒
        //If this fails we do not want to catch its exception, because failing in the first attempt means
        //whatever is being tested in `test` is not implemented correctly. Therefore we do not want to mask
        //the failure with a "Operation is not idempotent".
        test(firstResult)

        //This code will only be executed if the previous test succeed.
        //And now we want to catch the exception because if `test` fails here it means it is not idempotent.
        val remainingResults: Future[Seq[Either[ErrorCase, T]]] = Future.sequence {
          (1 until repetitions) map { _ =>
            Thread.sleep(250) // give enough time to lock and unlock in some testes, mainly release
            expect.run()
          }
        }

        remainingResults map { results ⇒
          try {
            results.foreach(test)
            succeed
          } catch {
            case e: TestFailedException =>
              val otherResultsString = (0 until repetitions - 1).map { i =>
                f"${i+1}%2d:\t${results(i)}"
              }.mkString("\n")
              throw new TestFailedException(s"""Operation is not idempotent. Results:
                                                |0:\t$firstResult
                                                |$otherResultsString
                                                |${e.message}""".stripMargin,
                e, e.failedCodeStackDepth + 1)
          }
        }
      }
    }

    def idempotentRightValue(testOnRight: T => Assertion): Future[Assertion] = idempotentTest(t => testOnRight(t.right.value))
    def idempotentLeftValue(testOnLeft: ErrorCase => Assertion): Future[Assertion] = idempotentTest(t => testOnLeft(t.left.value))

    def rightValueShouldIdempotentlyBe(rightValue: T): Future[Assertion] = idempotentRightValue(_ shouldBe rightValue)
    def rightValueShouldIdempotentlyBeUnit()(implicit ev: T =:= Unit): Future[Assertion] = idempotentRightValue(_.shouldBe(()))
    def leftValueShouldIdempotentlyBe(leftValue: ErrorCase): Future[Assertion] = idempotentLeftValue(_ shouldBe leftValue)
  }

}