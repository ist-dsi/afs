package pt.tecnico.dsi.afs

import com.typesafe.scalalogging.LazyLogging
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.exceptions.TestFailedException
import org.scalatest.{EitherValues, Matchers}
import work.martins.simon.expect.core.Expect

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt

trait LowPriorityImplicits extends ScalaFutures {
  implicit class SimpleRichExpect[T](expect: Expect[T]) {
    def value: T = expect.run().futureValue(PatienceConfig(
      timeout = scaled(expect.settings.timeout),
      interval = scaled(1000.millis)
    ))
  }
}

trait TestUtils extends ScalaFutures with Matchers with EitherValues with LazyLogging with LowPriorityImplicits {
  def idempotent[A, B](expect: Expect[Either[A, B]], repetitions: Int = 3)(test: Either[A, B] => Unit): Unit = {
    require(repetitions >= 2, "To test for idempotency at least 2 repetitions must be made")
    //If this fails we do not want to catch its exception, because failing in the first attempt means
    //whatever is being tested in `test` is not implemented correctly. Therefore we do not want to mask
    //the failure with a "Operation is not idempotent".
    val firstResult = expect.value
    test(firstResult)

    //This code will only be executed if the previous test succeed.
    //And now we want to catch the exception because if `test` fails here it means it is not idempotent.
    val results: IndexedSeq[Either[A, B]] = (1 until repetitions).map(_ => expect.value)
    try {
      results.foreach(test)
    } catch {
      case e: TestFailedException =>
        val otherResultsString = (1 until repetitions).map { i =>
          f"  $i:\t${results(i-1)}"
        }.mkString("\n")

        throw new TestFailedException(s"""Operation is not idempotent. Results:
                                          |  0:\t$firstResult
                                          |$otherResultsString
                                          |${e.message}""".stripMargin,
          e, e.failedCodeStackDepth + 1)
    }
  }

  implicit class RichExpect[A, B](expect: Expect[Either[A, B]]) extends SimpleRichExpect(expect) {
    def leftValue: A = value.left.value
    def rightValue: B = value.right.value
    def rightValueShouldBeUnit()(implicit ev: B =:= Unit): Unit = rightValue.shouldBe(())

    def leftValueShouldIdempotentlyBe(leftValue: A): Unit = idempotent(expect)(_.left.value shouldBe leftValue)
    def rightValueShouldIdempotentlyBe(rightValue: B): Unit = idempotent(expect)(_.right.value shouldBe rightValue)
    def rightValueShouldIdempotentlyBeUnit()(implicit ev: B =:= Unit): Unit = idempotent(expect)(_.right.value.shouldBe(()))

    def idempotentRightValue(rightValue: B => Unit): Unit = idempotent(expect)(t => rightValue(t.right.value))
  }
}