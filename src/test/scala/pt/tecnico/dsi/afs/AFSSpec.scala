package pt.tecnico.dsi.afs

import java.io.File

import squants.information.InformationConversions._
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{FlatSpec, Matchers}
import squants.information.{Gigabytes, Information, Kilobytes}
import work.martins.simon.expect.fluent.Expect

import scala.concurrent.Await
import scala.concurrent.duration.DurationInt
import scala.concurrent.ExecutionContext.Implicits.global

/*
/**
 *
 */
class AFSSpec extends FlatSpec with ScalaFutures with Matchers {
 val afs = new AFS()
 val path = "/afs/.example.com/"
 val randomFile = "random.data"
 val nonExistingPath = "/afs/aaaa"
 val volumeName: String = "root.cell"
 val nonExitingVolumeName = "root.a"
 val volumeExpected = "#" + volumeName
 //
 val ptsAdminUser = "afsadmin.admin"
 val ptsAdminUserId = "1"
 val ptsNonExistingUser = "test.admin"
 val ptsNonExistingUserId = "1024"

 val defaultQuota = Kilobytes(2048000)

 val timeout = 5.seconds



   removes all files and directories in the path afs directory

 def clean() = {
   sys.process.Process(Seq("rm", "-f", tmpFile), new java.io.File(path)).!!
   val command = s"fs setquota -path ${new File(path).getPath} -max ${defaultQuota.toKilobytes.toLong}"
   sys.process.Process(command.split(" "), new File(path)).!!

 }

  //region <PTS commands>


  "AFS listquota" should "return (name, quota, used) when the directory exists" in {
    val e = AFS.listquota(new File(path))
    e.run().futureValue match {
      case Right((outputVolumeName, quota, used)) => {
        // only the volume name is tested since we assume the fs commands works correctly
        assert(outputVolumeName == volumeName)
      }
      case _ => {
        assert(false)
      }
    }
  }
  it should "return error when directory does not exist" in {
    // TODO remove usage of listquota
    val e = AFS.listquota(new File(nonExistingPath))
    e.run().futureValue match {
      case Left(InvalidDirectory) => {
        assert(true)
      }
      case _ => {
        assert(false)
      }
    }
  }
  it should "return updated quota usave value after adding a file" in {
    var quotaBefore = Kilobytes(0)
    var usedBefore = Kilobytes(0)
    AFS.listquota(new File(path)).run().futureValue match {
      case Right((outputVolumeName, quota, used)) => {
        // only the volume name is tested since we assume the fs commands works correctly
        quotaBefore = quota
        usedBefore = used
      }
      case _ => {
        assert(false)
      }
    }
    // create a file of 1 kilobyte in test afs path
    val tmpFile = path + randomFile
    sys.process.Process(s"dd if=/dev/zero of=$tmpFile bs=2K count=1", new java.io.File(path)).!!
    // check if listquota outputs updated information
    AFS.listquota(new File(path)).run().futureValue match {
      case Right((outputVolumeName, quota, used)) => {
        assertResult(quotaBefore)(quota)
        assertResult(usedBefore + Kilobytes(2))(used)
      }
      case _ => {
        assert(false)
      }
    }
  }

  "AFS setquota" should "after increasing a volume quota it should return the same volume name and the new set quota" in {
    var quotaBefore = Kilobytes(0)
    afs.listquota(new File(path)).run().futureValue match {
      case Right((outputVolumeName, quota, used)) => {
        quotaBefore = quota
      }
      case _ => {
        assert(false)
      }
    }
    val res = afs.setQuota(new File(path), quotaBefore + 2.kilobytes).run().futureValue
    assertResult(Right(true))(res)
    afs.listquota(new File(path)).run().futureValue match {
      case Right((outputVolumeName, quota, used)) => {
        assertResult(quotaBefore + 2.kilobytes)(quota)
      }
      case _ => {
        assert(false)
      }
    }
  }

  it should "return error when directory does not exist" in {
    // TODO
    AFS.setQuota(new File(nonExistingPath), 1.kilobytes).run().futureValue match {
      case Left(errorCase) => {
        assertResult(InvalidDirectory)(errorCase)
      }
      case _ => {
        assert(false)
      }
    }
  }

  "AFS listMount" should "return the volume for the given directory" in {
      AFS.listMount(new File(path)).run().futureValue match {
      case Right(volume) => {
        assertResult(volumeExpected)(volume)
      }
      case _ => {
        fail("Did not get expected volume name")
      }
    }
  }

  it should "return invalid directory when given directory does not exist" in {
    AFS.listMount(new File(nonExistingPath)).run().futureValue match {
      case Left(error) => {
        assert(error == InvalidDirectory)
      }
      case _ => {
        fail("expected invalid directory but got something else")
      }
    }
  }



   "AFS MakeMount" should "create a volume at a given directory and return success" in {
     // TODO before after
     val ret = AFS.makeMount(new File(path), volumeName).run().futureValue match {
       case Right(success) => assert(success == true)
       case _ => fail("operation not successful")
     }
   }

           it should "return error when directory does not exist" in {
             // TODO before after
             val ret = AFS.makeMount(new File(nonExistingPath), volumeName).run().futureValue match {
               case Left(error) => assert(error == InvalidDirectory)
               case _ => fail("")
             }
           }
           it should "return error when the volume does not exist" in {
             // TODO before after
             val ret = AFS.makeMount(new File(path), nonExitingVolumeName).run().futureValue match {
               case Left(error) => assert(error == InvalidVolume)
               case _ => fail("")
             }
           }
           it should "return error when directory is already a mount point" in {
             // TODO before after
             AFS.makeMount(new File(path), volumeName).run().futureValue match {
               case Right(success) => assert(success == true)
               case _ => fail("operation not successful")
             }
             AFS.makeMount(new File(path), volumeName).run().futureValue match {
               case Left(error) => assert(error == DirectoryAlreadyMounting)
               case _ => fail("operation should have failed")
             }
           }



          "AFS RemoveMount" should "remove mount for the given directory" in {
            // TODO before after
            AFS.removeMount(new File(path)).run().futureValue match {
              case Right(success) => assert(success == true)
              case _ => "failed tol remove a mount"
            }
          }
          it should "return error if the directory does not exist" in {
            // TODO before after
          }
          it should "return error if the directory is not a mount point" in {
            // TODO before after
          }
          it should "return error when directory is already a mount point" in {
            // TODO
          }

          "AFS listACL" should "return the default ACL for the given directory" in {
            // TODO before after
          }
          it should "return error if the directory does not exists" in {
            // TODO before after
          }
          it should "return error if the directory is not a mount point" in {
            // TODO before after
          }

          "AFS setACL" should "change the ACL for the given directory" in {
            // TODO before after
          }
          it should "return error if the directory does not exists" in {
            // TODO before after
          }
          it should "return error if the directory is not a mount point" in {
            // TODO before after
          }


          "AFS checkVolumes" should "return success" in {
            // TODO before after
          }
          it should "return the specific error when volume is not ok" in {
            // TODO before after
          }

          //endregion

  "AFS getUserID" should "return the Id of an existing user" in {
    AFS.getUserId(ptsAdminUser).run().futureValue match {
      case Right(id) => {
        assertResult(ptsAdminUserId)(id)
      }
      case _ => {
        assert(false)
      }
    }
  }
  it should "fail when the user doesn't exist" in {
    AFS.getUserId(ptsNonExistingUser).run().futureValue match {
      case Left(error) => {
        assertResult(UnknownUserName)(error)
      }
      case _ => {
        assert(false)
      }
    }
  }


  "AFS getUserName" should "return the username of an existing user's id" in {
    AFS.getUserName(ptsAdminUserId).run().futureValue match {
      case Right(username) => {
        assertResult(ptsAdminUser)(username)
      }
      case _ => {
        assert(false)
      }
    }
  }
  it should "fail when the user's id doesn't exist" in {
    AFS.getUserName(ptsAdminUserId).run().futureValue match {
      case Left(error) => {
        assertResult(UnknownUserName)(error)
      }
      case _ => {
        assert(false)
      }
    }
  }
          //region <PTS commands>

          "AFS createUser" should "return success when username is valid and not used" in {
            //TODO
          }
          it should "fail when the username is invalid" in {
            //TODO
          }
          it should "fail when the username is already in use" in {
            //TODO
          }

          "AFS createGroup" should "create a group when user with username exists and group name is available" in {
            //TODO
          }
          it should "fail when user for username doest not exists" in {
            //TODO
          }
          it should "fail when group name already exists" in {
            //TODO
          }

          "AFS delete user or group" should "delete the given user/group if it exists" in {
            //TODO
          }
          it should "fail when given name is not a user nor group" in {
            //TODO
          }

          "AFS addUserToGroup" should "return success when user and group exist" in {
            //TODO
          }
          it should "fail when user does not exists and group does not exist" in {
            //TODO
          }
          it should "" in {
            //TODO
          }


          "AFS removeUserFromGroup" should "" in {
            //TODO
          }
          "AFS membership" should "" in {
            //TODO
          }
          "AFS listGroups " should "" in {
            //TODO
          }

          //endregion

          //region <VOS commands>

          "AFS backupVolume" should "" in {
            //TODO
          }
          "AFS createVolume" should "" in {
            //TODO
          }
          "AFS removeVolume" should "" in {
            //TODO
          }
          "AFS examineVolume" should "" in {
            //TODOl
          }
          "AFS releaseVolume" should "" in {
            //TODO
          }
          //endregion




}

*/