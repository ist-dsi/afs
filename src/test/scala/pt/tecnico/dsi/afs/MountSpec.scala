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

  // read write
  val readWriteMountPoint = new File("/afs/.example.com")
  val readWriteVolume = s"%$cellName:$volumeName"
  // regular
  val regularMountPoint = new File("/afs/example.com")
  val regularVolume = s"#$cellName:$volumeName"

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

  //"makeMount" should "mount a directory in the afs " in {}
  //"removeMount" should "remove the mount of a given directory" in {}

}
