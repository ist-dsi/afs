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
  val invalidVolumeName = "testt"

  "createVolume" should "create a volume normally" in {
    createVolume(server, partition, volumeName, 5.mebibytes).rightValue.shouldBe(())
  }
  it should "return Host Not Found when server does not exist" in {
    createVolume(invalidServer,partition,volumeName,5.mebibytes).leftValue.shouldBe(HostNotFound)
  }
  it should "return invalid partition when the given partition does not exist" in {
    createVolume(server,invalidPartition,volumeName,5.mebibytes).leftValue.shouldBe(InvalidPartition)
  }
  it should "return error when volume name already exists" in {
    createVolume(server,partition,volumeName,5.mebibytes).leftValue.shouldBe(InvalidVolumeName)
  }

  "removeVolume" should "remove normally" in {
    // CAREFULL the state from the createVolume test is used since we remove the volume created on those tests
    removeVolume(server,partition,volumeName).rightValue.shouldBe(())
  }
  it should "return Host Not Found when server does not exist" in {
    removeVolume(invalidServer,partition,volumeName).leftValue.shouldBe(HostNotFound)
  }
  it should "return invalid partition when the given partition does not exist" in {
    removeVolume(server,invalidPartition,volumeName).leftValue.shouldBe(InvalidPartition)
  }
  it should "return success when volume name already exists" in {
    removeVolume(server,partition,invalidVolumeName).rightValueShouldIdempotentlyBe(())
  }

  "addSite" should "create a replication site for the given volume" in {
    createVolume(server, partition, volumeName, 5.mebibytes).rightValue.shouldBe(())
    addSite(server,partition,volumeName).rightValue.shouldBe(())
  }
  it should "return success, the second time it is called with the same arguments" in {
    addSite(server,partition,volumeName).rightValueShouldIdempotentlyBe(())
  }
  it should "return error when the volumeName read/write does not exist" in {
    addSite(server,partition,invalidVolumeName).leftValue.shouldBe(NonExistantReadWriteVolume)
  }
  it should "return Host Not Found when server does not exist" in {
    addSite(invalidServer,partition,volumeName).leftValue.shouldBe(HostNotFound)
  }
  it should "return invalid partition when the given partition does not exist" in {
    addSite(server,invalidPartition,volumeName).leftValue.shouldBe(InvalidPartition)
  }

  "releaseVolume" should "release volume normally" in {
    releaseVolume(volumeName).rightValue.shouldBe(())
  }
}
