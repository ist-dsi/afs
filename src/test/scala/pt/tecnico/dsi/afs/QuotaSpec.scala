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
    listquota(nonExistingFile).leftValue shouldBe InvalidDirectory
  }
  it should "return updated quota usave value after adding a file" in {
    val (_, quotaBefore, usedBefore) = listquota(rootCellFile).rightValue

    val newFile = new File(rootCellFile, randomFile)
    val fileSize = 1.kibibytes
    val outputStream = new FileOutputStream(newFile)
    val data = Array.ofDim[Byte](fileSize.value.toInt)
    Random.nextBytes(data)
    outputStream.write(data)
    outputStream.close()

    val (_, quota, used) = listquota(rootCellFile).rightValue
    quota shouldBe quotaBefore
    used shouldBe (usedBefore + fileSize)
  }

  "setquota" should "return InvalidDirectory when directory does not exist" in {
    setQuota(nonExistingFile, 1.mebibytes).leftValue shouldBe InvalidDirectory
  }
  it should "update the quota to the requested value" in {
    val (_, quotaBefore, _) = listquota(rootCellFile).rightValue

    val newQuota = quotaBefore + 2.mebibytes
    setQuota(rootCellFile, newQuota).rightValueShouldBeUnit()

    listquota(rootCellFile).rightValue._2 shouldBe newQuota
  }

}
