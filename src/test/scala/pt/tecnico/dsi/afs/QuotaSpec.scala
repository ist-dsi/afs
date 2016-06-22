package pt.tecnico.dsi.afs

import java.io.{File, FileOutputStream}

import org.scalatest.{FlatSpec, Matchers}
import squants.information.InformationConversions._

import scala.util.Random

class QuotaSpec extends FlatSpec with TestUtils with Matchers{
  val afs = new AFS()
  import afs._

  val rootCellFile = new File("/afs/.example.com/")
  val nonExistingFile = new File("/afs/aaaa")
  val randomFile = "random.data"
  val volumeName = "root.cell"


  "listquota" should "return InvalidDirectory when directory does not exist" in {
    listQuota(nonExistingFile) leftValueShouldIdempotentlyBe InvalidDirectory
  }
  it should "return the quota and the used size" in {
    val Quota(_, quotaBefore, usedBefore) = listQuota(rootCellFile).rightValue
    val newFile = new File(rootCellFile, randomFile)
    val fileSize = 10.kibibytes
    val outputStream = new FileOutputStream(newFile)
    val data = Array.ofDim[Byte](fileSize.toBytes.toInt)
    Random.nextBytes(data)
    outputStream.write(data)
    outputStream.close()
    // This test is affected by some other test
    // TODO create a partitition dedicated to this test
    // TODO https://github.com/sbt/sbt/issues/882

    listQuota(rootCellFile).idempotentRightValue { case Quota(_, quota, used) =>
      quota.toKibibytes.toInt shouldBe quotaBefore.toKibibytes.toInt
      used.toKibibytes.toInt shouldBe ((usedBefore + fileSize).toKibibytes.toInt +- 1)
    }
  }

  "setquota" should "return InvalidDirectory when directory does not exist" in {
    setQuota(nonExistingFile, 1.mebibytes) leftValueShouldIdempotentlyBe InvalidDirectory
  }

  it should "update the quota to the requested value" in {
    val Quota(_, quotaBefore, _) = listQuota(rootCellFile).rightValue

    val newQuota = quotaBefore + 200.kibibytes
    setQuota(rootCellFile, newQuota).rightValueShouldIdempotentlyBeUnit()

    listQuota(rootCellFile).rightValue.quota shouldBe newQuota
  }
}
