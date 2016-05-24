package pt.tecnico.dsi.afs

import java.io.{File, FileOutputStream}

import org.scalatest.FlatSpec
import squants.information.InformationConversions._

import scala.util.Random

class QuotaSpec extends FlatSpec with TestUtils {
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
    val fileSize = 1.kibibytes
    val outputStream = new FileOutputStream(newFile)
    val data = Array.ofDim[Byte](fileSize.value.toInt)
    Random.nextBytes(data)
    outputStream.write(data)
    outputStream.close()

    listQuota(rootCellFile).idempotentRightValue { case Quota(_, quota, used) =>
      quota shouldBe quotaBefore
      used shouldBe (usedBefore + fileSize)
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
