package pt.tecnico.dsi.afs

import java.io.{File, FileOutputStream}

import org.scalatest.{AsyncFlatSpec, FlatSpec}
import squants.information.InformationConversions._

/**
  *
  */
class VolumeSpec extends AsyncFlatSpec with TestUtils {
  val afs = new AFS()

  import afs._

  val server = "afs"
  val invalidServer = "afs0"
  val partition = "/vicepa"
  val invalidPartition = "/vicepe"
  val volumeName = "test"
  val invalidVolumeName = "testt"

  "createVolume" should "create a volume successfully" in {
    createVolume(server, partition, volumeName, 5.mebibytes).rightValueShouldBeUnit()
  }
  it should "return Host Not Found when server does not exist" in {
    createVolume(invalidServer, partition, volumeName, 5.mebibytes).leftValueShouldIdempotentlyBe(HostNotFound)
  }
  it should "return invalid partition when the given partition does not exist" in {
    createVolume(server, invalidPartition, volumeName, 5.mebibytes).leftValueShouldIdempotentlyBe(InvalidPartition)
  }
  it should "return error when volume name already exists" in {
    createVolume(server, partition, volumeName, 5.mebibytes).leftValueShouldIdempotentlyBe(InvalidVolumeName)
  }

  "removeVolume" should "remove a volume successfully" in {
    // CAREFULL the state from the createVolume test is used since we remove the volume created on those tests
    removeVolume(server, partition, volumeName).rightValueShouldIdempotentlyBeUnit()
  }
  it should "return Host Not Found when server does not exist" in {
    removeVolume(invalidServer, partition, volumeName).leftValueShouldIdempotentlyBe(HostNotFound)
  }
  it should "return invalid partition when the given partition does not exist" in {
    removeVolume(server, invalidPartition, volumeName).leftValueShouldIdempotentlyBe(InvalidPartition)
  }
  it should "return success when when volume does not exist" in {
    removeVolume(server, partition, invalidVolumeName).rightValueShouldIdempotentlyBeUnit()
  }

  "addSite" should "create a replication site for the given volume successfully" in {
    createVolume(server, partition, volumeName, 5.mebibytes).rightValueShouldBeUnit().flatMap {
      case _ =>
        addSite(server, partition, volumeName).rightValueShouldBeUnit().map {
          case future => future
        }
    }
  }
  it should "return error when the volumeName read/write does not exist" in {
    addSite(server, partition, invalidVolumeName).leftValueShouldIdempotentlyBe(NonExistingVolume)
  }
  it should "return Host Not Found when server does not exist" in {
    addSite(invalidServer, partition, volumeName).leftValueShouldIdempotentlyBe(HostNotFound)
  }
  it should "return invalid partition when the given partition does not exist" in {
    addSite(server, invalidPartition, volumeName).leftValueShouldIdempotentlyBe(InvalidPartition)
  }

  "releaseVolume" should "release volume normally" in {
    releaseVolume(volumeName).rightValueShouldBeUnit()
  }
  it should "return error when volume does not exist" in {
    releaseVolume(invalidVolumeName).leftValueShouldIdempotentlyBe(NonExistingVolume)
  }

  "examineVolume" should "check that volume is working" in {
    examineVolume(volumeName).rightValueShouldBe(())
  }
  it should "return error when volume does not exist" in {
    examineVolume(invalidVolumeName).leftValueShouldIdempotentlyBe(NonExistingVolume)
  }

  "volumeExist" should "return success idempotently when volume exists" in {
    volumeExists(volumeName, server).rightValueShouldBeUnit()
  }
  it should "return error when volume does not exist" in {
    volumeExists(invalidVolumeName, server).leftValueShouldIdempotentlyBe(NonExistingVolume)
  }

  "backupVolume" should "return success when backup volume is successfully created" in {
    backupVolume(volumeName).rightValueShouldBeUnit()
  }
  it should "return error when the target volume does not exist" in {
    backupVolume(invalidVolumeName).leftValueShouldIdempotentlyBe(NonExistingVolume)
  }


}
