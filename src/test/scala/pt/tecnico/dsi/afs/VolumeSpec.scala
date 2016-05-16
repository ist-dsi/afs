package pt.tecnico.dsi.afs

import java.io.{File, FileOutputStream}

import org.scalatest.FlatSpec
import squants.information.InformationConversions._

/**
  *
  */
class VolumeSpec extends FlatSpec with TestUtils {
  val afs = new AFS()
  import afs._

  val server = "afs"
  val invalidServer = "afs0"
  val partition = "/vicepa"
  val invalidPartition = "/vicepe"
  val volumeName = "test"
  val invalidVolumeName = "test"

  "createVolume" should "create a volume normally" in {
    createVolume(server, partition, volumeName, 5.mebibytes).rightValue shouldBe true
  }

}
