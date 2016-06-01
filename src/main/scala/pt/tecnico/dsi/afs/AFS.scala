package pt.tecnico.dsi.afs

import java.io.File

import com.typesafe.config.Config
import com.typesafe.scalalogging.LazyLogging
import squants.information.InformationConversions._
import squants.information.Information
import work.martins.simon.expect.EndOfFile

import scala.util.matching.Regex.Match
import work.martins.simon.expect.fluent.{Expect, StringWhen}
import pt.tecnico.dsi.afs.AFSUtils._

class AFS(val settings: Settings = new Settings()) extends LazyLogging {
  def this(config: Config) = this(new Settings(config))

  import settings._

  //region <FS commands>
  /** */
  def listQuota(directory: File): Expect[Either[ErrorCase, Quota]] = {
    val dir = directory.getPath
    val e = new Expect(s"fs listquota -path $dir", defaultUnknownError[Quota])
    e.expect
      .addWhen(insufficientPermission)
      .addWhen(invalidDirectory)
      .when(
        """Volume Name\s+Quota\s+Used\s+%Used\s+Partition
          |([^\s]+)\s+(\d+|no limit)\s+(\d+)""".stripMargin.r)
      .returning { m: Match =>
        val quotaDouble = if (m.group(2) == "no limit") Double.PositiveInfinity else m.group(2).toDouble
        //Quota and Used are in kibibytes contrary to what is stated in the documentation
        Right(Quota(m.group(1), quotaDouble.kibibytes, m.group(3).toDouble.kibibytes))
      }
      .addWhen(unknownError)
    e
  }

  def setQuota(directory: File, quota: Information): Expect[Either[ErrorCase, Unit]] = {
    require((quota > 0.kibibytes && quota <= 2.tebibytes) || quota.value.isPosInfinity,
      "new quota must be ]0 Kib, 2 Tib] ∪ +∞")
    val newQuota = if (quota.value.isPosInfinity) "0" else s"${quota.toKibibytes.round}"
    val e = new Expect(s"fs setquota -path ${directory.getPath} -max ${newQuota}K", defaultUnknownError[Unit])
    e.expect
      // TODO permissions not tested
      .addWhen(insufficientPermission)
      .addWhen(invalidDirectory)
      .addWhen(successOnEndOfFile)
      .addWhen(unknownError)
    e
  }

  /**
    * For more details see "man fs_lsmount"
    *
    * @param directory
    * @return
    */
  def listMount(directory: File): Expect[Either[ErrorCase, String]] = {
    val e = new Expect(s"fs lsmount ${directory.getPath}", defaultUnknownError[String])
    e.expect
      .addWhen(insufficientPermission)
      .addWhen(invalidDirectory)
      .addWhen(notAMountPoint)
      .when(s"a mount point for volume '([^']+)'".r)
      .returning(m => Right(m.group(1)))
      .addWhen(unknownError)
    e
  }

  def makeMount(directory: File, volume: String): Expect[Either[ErrorCase, Unit]] = {
    val e = new Expect(s"fs mkmount -dir ${directory.getPath} -vol $volume", defaultUnknownError[Unit])
    e.expect
      .addWhen(insufficientPermission)
      .when("File exists")
      .returningExpect {
        /*listMount(directory) transform {
          case Left(NotAMountPoint) =>
            //The existing directory is NOT a mount point. We must return an error.
            Left(FileAlreadyExists)
          case Right(v) if v.contains(volume) =>
            //The existing directory is already a mount point to the volume we want
            Right(())
          case Left(l) => Left(l)
          case Right(v) =>
            //The existing directory is a mount point to another volume
            //We must remove the existing mount point and then invoke makeMount again
            removeMount(directory) transform {
              case Right(()) => makeMount(directory)
              case either => either
            }
        }*/
        ???
      }
      .when(s"volume $volume does not exist")
      .returning(Left(InvalidVolume))
      .addWhen(successOnEndOfFile)
    e
  }

  def removeMount(directory: File): Expect[Either[ErrorCase, Unit]] = {
    val e = new Expect(s"fs rmmount -dir ${directory.getPath}", defaultUnknownError[Unit])
    e.expect
      .addWhen(invalidDirectory)
      .returning(Right(())) //This makes it idempotent
      .addWhen(successOnEndOfFile)
      .addWhen(unknownError)
    e
  }

  /**
    * Returns the access control list (ACL) associated with the specified file, directory, or symbolic link.
    * Check 'man fs_listacl"
    *
    * @param directory File representing the specified target
    * @return In case of success, a Right that includes Map from user to granted permissions.
    *         In case of invalid directory it returns Left(InvalidDirectory)
    */
  def listACL(directory: File): Expect[Either[ErrorCase, (Map[String, Permission], Map[String, Permission])]] = {
    val dir = directory.getPath
    val e = new Expect(s"fs listacl -path $dir", defaultUnknownError[(Map[String, Permission], Map[String, Permission])])
    e.expect
      .addWhen(invalidDirectory)
      .when((s"Access list for $dir" +
        """ is
          |Normal rights:
          |((?:\s+[\w:.]+ \w+\n?)+)
          |(?:Negative rights:
          |((?:\s+[\w:.]+ \w+\n?)+))?""").stripMargin.r) //user or group follow the format "system:user1"
      .returning { m: Match =>
      val normalPermissions = m.group(1).split('\n').map { acl =>
        val Array(userOrGroup, permissionAcl, _*) = acl.trim.split(" ")
        (userOrGroup, Permission(permissionAcl))
      }.toMap
      val negativePermission = Option(m.group(2)).map { g => g.split('\n').map { acl =>
        val Array(userOrGroup, permissionAcl, _*) = acl.trim.split(" ")
        (userOrGroup, Permission(permissionAcl))
      }.toMap
      }

      val ret = (normalPermissions, negativePermission.getOrElse(Map()))
      logger.info(ret.toString)
      Right(ret)
    }
    e
  }

  /**
    * Adds and overrides the access control list (ACL) associated with the specified file, directory or symbolic link.
    * If negative flag is true then the negatives rights are changed instead of the normal right
    * Check 'man fs_listacl"
    *
    * @param directory File representing the specified target
    * @param ACLs
    * @return
    */
  def setACL(directory: File, ACLs: Map[String, Permission], negative: Boolean = false): Expect[Either[ErrorCase, Unit]] = {
    val ACLstring = ACLs.map(t => t._1 + " " + t._2.acl).mkString(" ")
    val cmd = s"fs setacl -dir ${directory.getPath} -acl $ACLstring" + (if(negative) " -negative" else "")
    logger.debug(cmd)
    val e = new Expect(cmd, defaultUnknownError[Unit])
    e.expect
      .addWhen(invalidDirectory)
      .when("Tried to add non-existent user to access control list")
      .returning(Left(InvalidUserOrGroupName))
        .when("You can not change a backup or readonly volume".r)
        .returning(Left(CanNotChangeReadOnlyVolume))
      .when(EndOfFile)
      .returning(Right(()))
    e
  }

  def checkVolumes(): Expect[Either[ErrorCase, Unit]] = {
    val e = new Expect(s"fs checkvolumes", defaultUnknownError[Unit])
    e.expect("All volumeID/name mappings checked.")
      .returning(Right(()))
    e
  }

  //endregion

  //region <PTS commands>
  /**
    * Get the user's id for the given username
    *
    * @param username
    * @return
    */
  def getUserId(username: String): Expect[Either[ErrorCase, String]] = {
    val e = new Expect(s"pts examine -nameorid $username", defaultUnknownError[String])
    e.expect
      .when("""id: (\d+)""".r)
      .returning { m => Right(m.group(1)) }
      .when("""User or group doesn't exist""".r)
      .returning(Left(UnknownUserName))
    e
  }

  /**
    * Get the user name for the given user's ids
    *
    * @param id
    * @return
    */
  def getUserName(id: String): Expect[Either[ErrorCase, String]] = {
    val e = new Expect(s"pts examine -nameorid $id", defaultUnknownError[String])
    e.expect
      .when("""Name: ([\w\.]+),""".r)
      .returning { m =>
        Right(m.group(1))
      }
      .when("""User or group doesn't exist""".r)
      .returning(Left(UnknownUserId))
    e
  }

  def createUser(name: String, afsId: Int): Expect[Either[ErrorCase, Unit]] = {
    val e = new Expect(s"pts createuser -name $name -id $afsId", defaultUnknownError[Unit])
    e.expect
      .when(s"Entry for id already exists ; unable to create user '$name' with id '$afsId'")
      .returning(Left(AFSIdAlreadyTaken))
      .when(s"Badly formed name (group prefix doesn't match owner?) ; unable to create user '$name' with id '$afsId'")
      .returning(Left(InvalidUserName))
      .when(s"User '$name' has id '$afsId'")
      .returning(Right(()))
    e
  }

  def createGroup(name: String, owner: String): Expect[Either[ErrorCase, Int]] = {
    val e = new Expect(s"pts creategroup -name $name -owner $owner", defaultUnknownError[Int])
    e.expect
      .when(s"""User or group doesn't exist ; unable to create group $name with id -?\d+ owned by $owner'""".r)
      .returning(Left(InvalidUserName))
      .when(s"""group $name has id (-\d+)""".r)
      .returning { m: Match => Right(m.group(1).toInt) }
    e
  }

  def deleteUserOrGroup(name: String): Expect[Either[ErrorCase, Unit]] = {
    //Should we also call fs cleanacl?
    val e = new Expect(s"pts delete -nameorid $name", defaultUnknownError[Unit])
    e.expect
      .when(s"User or group doesn't exist so couldn't look up id for $name")
      .returning(Left(InvalidUserOrGroupName))
      .when(EndOfFile)
      .returning(Right(()))
    e
  }

  def addUserToGroup(name: String, group: String): Expect[Either[ErrorCase, Unit]] = {
    val e = new Expect(s"pts adduser -user $name -group $group", defaultUnknownError[Unit])
    e.expect
      .when(s"User or group doesn't exist ; unable to add user $name to group $group")
      .returning(Left(InvalidUserOrGroupName))
      .when(EndOfFile)
      .returning(Right(()))
    e
  }

  def removeUserFromGroup(name: String, group: String): Expect[Either[ErrorCase, Unit]] = {
    val e = new Expect(s"pts removeuser -user $name -group $group", defaultUnknownError[Unit])
    e.expect
      .when(s"User or group doesn't exist ; unable to remove user $name to group $group")
      .returning(Left(InvalidUserOrGroupName))
      .when(EndOfFile)
      .returning(Right(()))
    e
  }

  def membership(name: String): Expect[Either[ErrorCase, Set[String]]] = {
    val e = new Expect(s"pts membership -nameorid $name", defaultUnknownError[Set[String]])
    e.expect
      .when(s"User or group doesn't exist")
      .returning(Left(InvalidUserOrGroupName))
      //If $name is a user name or user afs id
      .when(
      s"""Groups [^ ]+ id: \d+ is a member of:
          |(  [^\n]+\n?)+""".stripMargin.r)
      .returning { m: Match =>
        val groups = m.group(0).split('\n').map(_.trim).toSet
        Right(groups)
      }
      //If $name is a group name or group afs id
      .when(
      s"""Members of group [^ ]+ id: -\d+ are:
          |(  [^\n]+\n?)+""".stripMargin.r)
      .returning { m: Match =>
        val users = m.group(0).split('\n').map(_.trim).toSet
        Right(users)
      }
    e
  }

  def listGroups(): Expect[Either[ErrorCase, Seq[(String, Int, Int, Int)]]] = {
    val e = new Expect(s"pts listentries -groups", defaultUnknownError[Seq[(String, Int, Int, Int)]])
    e.expect
      .when(
        s"""Name\tID\tOwner\tCreator
            |([^\n]+\n?)+""".stripMargin.r)
      .returning { m: Match =>
        val groups = m.group(0).split('\n').map { groupString =>
          val Array(name, id, owner, creator) = groupString.trim.split("""\s+""")
          (name, id.toInt, owner.toInt, creator.toInt)
        }.toSeq
        Right(groups)
      }
    e
  }

  //endregion

  //region <VOS commands>
  /**
    * Create an AFS volume on a given "server" , in given a "partition" in the given server, with a given
    * "name" and a maximum quota defined by "maxQuota"
    *
    * @param server    hostname of the afs server
    * @param partition name of the partition on the afs server in which the volume is being created
    * @param name      name of the volume being created
    * @param maxQuota  defines the maximum allowed quota, the minimum value is 1 kibibyte
    * @return /TODO
    */
  def createVolume(server: String, partition: String, name: String, maxQuota: Information): Expect[Either[ErrorCase, Unit]] = {
    // TODO turn method idempotent
    // TODO If called twice with same name(server,partition), check that quota is the same
    val e = new Expect(
      s"vos create -server $server -partition $partition -name $name -maxquota ${maxQuota.toKibibytes.toLong}K",
      defaultUnknownError[Unit])
    e.expect
      .addWhen(hostNotFound)
      .addWhen(invalidPartition)
      .when("already exists".r)
      .returning(Left(InvalidVolumeName))
      .when("bad integer specified for quota".r)
      .returning(Left(InvalidVolumeQuota))
      .when(s"""Volume \\d+ created on partition $partition of $server""".r)
      .returning(Right(()))
      .addWhen(unknownError)
    e
  }

  /**
    * Remove an AFS volume on a given "server" , in given a "partition" in the given server, with a given
    * "name"
    *
    * This function is idempotent
    *
    * @param server    name where the AFS's partition is setup
    * @param partition where the AFS volume was created
    * @param name      of an existing AFS volume name
    * @return return unit when volume is removed or when it does not exist
    */
  def removeVolume(server: String, partition: String, name: String): Expect[Either[ErrorCase, Unit]] = {
    val e = new Expect(s"vos remove -server $server -partition $partition -id $name",
      defaultUnknownError[Unit])

    e.expect
      .addWhen(hostNotFound)
      .addWhen(invalidPartition)
      .when("Can't find volume")
      .returning(Right(()))
      .when("Volume .+ deleted".r)
      .returning(Right(()))
      .addWhen(unknownError)
    e
  }

  /**
    *
    * @param server
    * @param partition
    * @param name
    * @return
    */
  def addSite(server: String, partition: String, name: String): Expect[Either[ErrorCase, Unit]] = {
    val e = new Expect(s"vos addsite -server $server -partition $partition -id $name",
      defaultUnknownError[Unit])
    e.expect
      .addWhen(hostNotFound)
      .addWhen(invalidPartition)
      .addWhen(nonExistingVolume)
      .when("RO already exists on partition".r)
      .returning(Right(()))
      .when("Added replication site".r)
      .returning(Right(()))
      .addWhen(unknownError)
    e
  }

  /**
    * Idempotent function that releases an AFS volume (see "man vos release")
    *
    * @param name
    * @return
    */
  def releaseVolume(name: String): Expect[Either[ErrorCase, Unit]] = {
    val e = new Expect(s"vos release -id $name", defaultUnknownError[Unit])
    e.expect
      .addWhen(nonExistingVolume)
      .when("Released volume .+ successfully".r)
      .returning(Right(()))
      .addWhen(unknownError)
    e
  }

  /**
    * Checks if server exist and is On-line
    *
    * @param name
    * @return
    */
  def examineVolume(name: String): Expect[Either[ErrorCase, Unit]] = {
    val e = new Expect(s"vos examine -id $name", defaultUnknownError[Unit])
    e.expect
      .addWhen(nonExistingVolume)
      .when("On-line".r)
      .returning(Right(()))
      .addWhen(unknownError)
    e
  }

  /**
    * Check if the given volume exists
    *
    * @param name
    * @return
    */
  def volumeExists(name: String): Expect[Either[ErrorCase, Unit]] = {
    val e = new Expect(s"vos listvldb -name $name", defaultUnknownError[Unit])
    e.expect
      .addWhen(nonExistingVolume)
      .when("(.+)".r)
      .returning(Right(()))
    e
  }

  /**
    * Creates a backup volume according to "man vos_backup"
    *
    * @param name
    * @return Unit when the volume was created successfully
    */
  def backupVolume(name: String): Expect[Either[ErrorCase, Unit]] = {
    val e = new Expect(s"vos backup -id $name", defaultUnknownError[Unit])
    e.expect
      .addWhen(nonExistingVolume)
      .when("Created backup volume".r)
      .returning(Right(()))
    e
  }

  //endregion
}