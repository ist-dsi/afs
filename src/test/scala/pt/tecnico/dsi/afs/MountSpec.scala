package pt.tecnico.dsi.afs

import java.io.{File, FileOutputStream}

import org.scalatest.FlatSpec
import squants.information.InformationConversions._

import scala.util.Random

class MountSpec extends FlatSpec with TestUtils {
  val afs = new AFS()
  import afs._

  val cellName = "example.com"
  val nonExistingDirectory = new File("/afs/aaaa")
  val nonMountPointDirectory = new File("/afs")
  val randomFile = "random.data"
  val volumeName = "root.cell"

  // read/write
  val readWriteMountPoint = new File("/afs/.example.com")
  // regular
  val regularMountPoint = new File("/afs/example.com")

  val regularVolume = s"#$cellName:$volumeName"
  val readWriteVolume = s"%$cellName:$volumeName"

  /*
  "listMount" should "return InvalidDirectory when directory does not exist" in {
    listMount(nonExistingDirectory) leftValueShouldIdempotentlyBe  InvalidDirectory
  }
  it should "return invalid mount Point when the existing directory is not a mount point" in{
    listMount(nonMountPointDirectory) leftValueShouldIdempotentlyBe  NotAMountPoint
  }
  it should "return the respective read write volume" in {
    listMount(readWriteMountPoint) rightValueShouldIdempotentlyBe readWriteVolume
  }
  it should "return the respective regular volume" in {
    listMount(regularMountPoint) rightValueShouldIdempotentlyBe  regularVolume
  }
  */

  val server = "afs"
  val invalidServer = "afs0"
  val partition = "/vicepa"
  val newVolumeName1 = "test1"
  val newVolumeName2 = "test2"
  val invalidVolume = "oioi"

  val validNewMountPoint1 = new File("/afs/.example.com/t1")
  val validNewMountPoint2 = new File("/afs/.example.com/t2")
  val validNewMountPoint3 = new File("/afs/.example.com/t3")
  val invalidNewMountPoint = new File("/afs/.example.com/aaa")
  "makeMount" should "mount directory successfully" in {
    createVolume(server, partition, newVolumeName1, 5.mebibytes) rightValueShouldBeUnit()

    makeMount(validNewMountPoint1, newVolumeName1) rightValueShouldIdempotentlyBeUnit()
  }
  it should "return error when the mount point directory already exists" in {
    validNewMountPoint2.createNewFile()
    makeMount(validNewMountPoint2, newVolumeName1).leftValueShouldIdempotentlyBe(FileAlreadyExists)
  }
  it should "return success when the mount point exists but for a different volume" in {
    createVolume(server, partition, newVolumeName2, 5.mebibytes) rightValueShouldBeUnit()
    makeMount(validNewMountPoint1, newVolumeName2) rightValueShouldIdempotentlyBeUnit()
  }
  it should "return error when volume does not exist" in {
    makeMount(validNewMountPoint3, invalidVolume).leftValueShouldIdempotentlyBe(InvalidVolume)
  }

  "removeMount" should "remove mount successfully" in {
    removeMount(validNewMountPoint1) rightValueShouldBeUnit()

    listMount(validNewMountPoint1).leftValue.shouldBe(InvalidDirectory)
  }
  // TODO check other cases

  "checkVolumes" should "remove mount successfully" in {
    checkVolumes() rightValueShouldIdempotentlyBeUnit()
  }

}
