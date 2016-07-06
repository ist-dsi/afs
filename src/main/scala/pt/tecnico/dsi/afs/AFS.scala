package pt.tecnico.dsi.afs

import java.io.File

import com.typesafe.config.Config
import com.typesafe.scalalogging.LazyLogging
import squants.information.InformationConversions._
import squants.information.Information
import work.martins.simon.expect.EndOfFile

import work.martins.simon.expect.core.Expect
import work.martins.simon.expect.fluent.{Expect ⇒ FluentExpect, fluentToCoreExpect}
import pt.tecnico.dsi.afs.AFSUtils._

class AFS(val settings: Settings = new Settings()) extends LazyLogging {
  def this(config: Config) = this(new Settings(config))

  import settings._

  //region <FS commands>
  /**
    * For more details see "man fs_listquota"
    *
    * @param directory
    * @return
    */
  def listQuota(directory: File): Expect[Either[ErrorCase, Quota]] = {
    val dir = directory.getPath
    val e = new FluentExpect(s"fs listquota -path $dir", defaultUnknownError[Quota])
    e.expect
      .addWhen(insufficientPermission)
      .addWhen(invalidDirectory)
      .when(
        """Volume Name\s+Quota\s+Used\s+%Used\s+Partition
          |([^\s]+)\s+(\d+|no limit)\s+(\d+)""".stripMargin.r)
        .returning { m =>
          val quotaDouble = if (m.group(2) == "no limit") Double.PositiveInfinity else m.group(2).toDouble
          //Quota and Used are in kibibytes contrary to what is stated in the documentation
          Right(Quota(m.group(1), quotaDouble.kibibytes, m.group(3).toDouble.kibibytes))
        }
      .addWhen(unknownError)
    e
  }

  /**
    * For more details see "man fs_setquota"
    *
    * @param directory
    * @param quota
    * @return
    */
  def setQuota(directory: File, quota: Information): Expect[Either[ErrorCase, Unit]] = {
    require((quota > 0.kibibytes && quota <= 2.tebibytes) || quota.value.isPosInfinity,
      "new quota must be ]0 Kib, 2 Tib] ∪ +∞")
    val newQuota = if (quota.value.isPosInfinity) "0" else s"${quota.toKibibytes.round}"
    val e = new FluentExpect(s"fs setquota -path ${directory.getPath} -max ${newQuota}K", defaultUnknownError[Unit])
    e.expect
      // TODO permissions not tested
      .addWhen(insufficientPermission)
      .addWhen(invalidDirectory)
      .addWhen(unknownError)
      .addWhen(successOnEndOfFile)
    e
  }

  /**
    * For more details see "man fs_lsmount"
    *
    * @param directory
    * @return
    */
  def listMount(directory: File): Expect[Either[ErrorCase, String]] = {
    val e = new FluentExpect(s"fs lsmount -dir ${directory.getPath}", defaultUnknownError[String])
    e.expect
      .addWhen(insufficientPermission)
      .addWhen(invalidDirectory)
      .addWhen(notAMountPoint)
      .when(s"a mount point for volume '([^']+)'".r)
        .returning(m => Right(m.group(1)))
      .addWhen(unknownError)
    e
  }

  /**
    * For more details see "man fs_mkmount"
    * @param directory
    * @param volume
    * @return
    */
  def makeMount(directory: File, volume: String): Expect[Either[ErrorCase, Unit]] = {
    val e = new FluentExpect(s"fs mkmount -dir ${directory.getPath} -vol $volume", defaultUnknownError[Unit])
    //@formatter:off
    e.expect
      .addWhen(insufficientPermission)
      .when(s"'${directory.getAbsolutePath}': File exists")
        //The file already exists lets see if it is a mount point
        .returningExpect {
          listMount(directory).transform {
            case Right(d) if !d.contains(volume)=>
              //The file is a mount point but to a different volume
              //So we first remove the mount and then make the mount to the pretended volume
              removeMount(directory).transform {
                case Right(()) => makeMount(directory, volume)
              } {
                case Left(l) => Left(l)
              }
          } {
            //The file is not a mount point, so we just return FileAlreadyExists
            case Left(NotAMountPoint) => Left(FileAlreadyExists)
            case Left(l) => Left(l)
            //The file is already a mount point to the volume we want. So we dont do anything.
            case Right(d) if d.contains(volume) => Right(())
          }
        }
      .when(s"volume $volume does not exist")
        .returning(Left(InvalidVolume))
      .addWhen(unknownError)
      .addWhen(successOnEndOfFile)
    //@formatter:on
    e
  }

  /**
    * For more details see "man fs_rmmount"
    *
    * @param directory
    * @return
    */
  def removeMount(directory: File): Expect[Either[ErrorCase, Unit]] = {
    val e = new FluentExpect(s"fs rmmount -dir ${directory.getPath}", defaultUnknownError[Unit])
    e.expect
      .addWhen(invalidDirectory)
        //flush afs caches to so that makeMount recognizes that the mount point is free.
        //We use flushall instead of the other flush commands because the other commands did not work.
        .returningExpect(flushAll())
      .addWhen(unknownError)
      .when(EndOfFile)
        .returningExpect(flushAll())
    e
  }

  /**
    * Forces the Cache Manager to update volume information
    * see "man fs_checkvolumes"
    *
    * @return
    */
  def checkVolumes(): Expect[Either[ErrorCase, Unit]] = {
    val e = new FluentExpect(s"fs checkvolumes", defaultUnknownError[Unit])
    e.expect
      .when("All volumeID/name mappings checked.")
        .returning(Right(()))
      .addWhen(unknownError)
    e
  }

  /**
    * Forces the Cache Manager to update volume information
    * see "fs help"
    *
    * @return
    */
  def flushAll(): Expect[Either[ErrorCase, Unit]] = {
    val e = new FluentExpect(s"fs flushall", defaultUnknownError[Unit])
    e.expect
      .addWhen(unknownError)
      .addWhen(successOnEndOfFile)
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
    val e = new FluentExpect(s"fs listacl -path $dir", defaultUnknownError[(Map[String, Permission], Map[String, Permission])])
    e.expect
      .addWhen(invalidDirectory)
      .when(
        """Normal rights:
          |((?:\s+[\w:.]+ \w+\n?)+)
          |(?:Negative rights:
          |((?:\s+[\w:.]+ \w+\n?)+))?""".stripMargin.r) //user or group follow the format "system:user1"
        .returning { m =>
          val normalPermissions = m.group(1).split('\n').map { acl =>
            val Array(userOrGroup, permissionAcl, _*) = acl.trim.split(" ")
            (userOrGroup, Permission(permissionAcl))
          }.toMap
          val negativePermission = Option(m.group(2)).map { g =>
            g.split('\n').map { acl =>
              val Array(userOrGroup, permissionAcl, _*) = acl.trim.split(" ")
              (userOrGroup, Permission(permissionAcl))
            }.toMap
          }

          Right((normalPermissions, negativePermission.getOrElse(Map.empty)))
        }
      .addWhen(unknownError)
    e
  }

  /**
    * Adds and overrides the access control list (ACL) associated with the specified file, directory or symbolic link.
    * If negative flag is true then the negatives rights are add instead of the normal rights.
    *
    * Check 'man fs_listacl"
    *
    * @param directory File representing the specified target
    * @param ACLs
    * @return
    */
  def setACL(directory: File, ACLs: Map[String, Permission], negative: Boolean = false): Expect[Either[ErrorCase, Unit]] = {
    val ACLstring = ACLs.map(t => t._1 + " " + t._2.acl).mkString(" ")
    val e = new FluentExpect(
      s"fs setacl -dir ${directory.getPath} -acl $ACLstring" + (if (negative) " -negative" else ""),
      defaultUnknownError[Unit])
    e.expect
      .addWhen(invalidDirectory)
      .when("Tried to add non-existent user to access control list")
        .returning(Left(InvalidUserOrGroupName))
      .when("You can not change a backup or readonly volume".r)
        .returning(Left(CanNotChangeReadOnlyVolume))
      .addWhen(unknownError)
      .addWhen(successOnEndOfFile)
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
  def getGroupOrUserId(username: String): Expect[Either[ErrorCase, Int]] = {
    val e = new FluentExpect(s"pts examine -nameorid $username", defaultUnknownError[Int])
    e.expect
      .when("""id: (\d+)""".r)
        .returning(m ⇒ Right(m.group(1).toInt))
      .when("""User or group doesn't exist""".r)
        .returning(Left(UnknownUserName))
      .addWhen(unknownError)
    e
  }

  /**
    * Get the user name for the given user's ids
    *
    * @param id
    * @return
    */
  def getGroupOrUserName(id: Int): Expect[Either[ErrorCase, String]] = {
    val e = new FluentExpect(s"pts examine -nameorid $id", defaultUnknownError[String])
    e.expect
      .when("""Name: ([\w\.]+),""".r)
        .returning(m ⇒ Right(m.group(1)))
      .when("pts: User or group doesn't exist")
        .returning(Left(UnknownUserOrGroupId))
      .addWhen(unknownError)
    e
  }

  def createUser(name: String, afsId: Int): Expect[Either[ErrorCase, Unit]] = {
    val e = new FluentExpect(s"pts createuser -name $name -id $afsId", defaultUnknownError[Unit])
    e.expect
      .when(s"Entry for id already exists ; unable to create user $name with id $afsId")
        .returning(Left(AFSIdAlreadyTaken))
      .when(s"Entry for name already exists ; unable to create user $name with id $afsId")
        .returning(Left(UserNameAlreadyTaken))
      .when(s"Badly formed name (group prefix doesn't match owner?) ; unable to create user $name with id $afsId")
        .returning(Left(BadlyFormedUserName))
      .when(s"User $name has id $afsId")
        .returning(Right(()))
      .addWhen(unknownError)
    e
  }

  def createGroup(name: String, owner: String): Expect[Either[ErrorCase, Int]] = {
    val e = new FluentExpect(s"pts creategroup -name $name -owner $owner", defaultUnknownError[Int])
    e.expect
      .when(s"User or group doesn't exist ; unable to create group $name with id -?\\d+ owned by '$owner'".r)
        .returning(Left(UnknownUserName))
      .when(s"Entry for name already exists ; unable to create group $name with id -?\\d+ owned by '$owner'".r)
        .returning(Left(GroupNameAlreadyTaken))
      .when(s"group $name has id (-\\d+)".r)
        .returning(m => Right(m.group(1).toInt))
      .addWhen(unknownError)
    e
  }

  def deleteUserOrGroup(name: String): Expect[Either[ErrorCase, Unit]] = {
    //Should we also call fs cleanacl? But fs cleanacl target is an afs directory.
    //Should we call fs cleanacl for directory that has the user or group in the ACL's
    val e = new FluentExpect(s"pts delete -nameorid $name", defaultUnknownError[Unit])
    e.expect
      .when(s"User or group doesn't exist")
        .returning(Right(()))
      .when(EndOfFile)
        .returning(Right(()))
    e
  }

  def addUserToGroup(name: String, group: String): Expect[Either[ErrorCase, Unit]] = {
    val e = new FluentExpect(s"pts adduser -user $name -group $group", defaultUnknownError[Unit])
    e.expect
      .when(s"User or group doesn't exist")
        .returning(Left(InvalidUserOrGroupName))
      .when("Entry for id already exists")
        .returning(Right(()))
      .when(EndOfFile)
        .returning(Right(()))
    e
  }

  def removeUserFromGroup(name: String, group: String): Expect[Either[ErrorCase, Unit]] = {
    val e = new FluentExpect(s"pts removeuser -user $name -group $group", defaultUnknownError[Unit])
    e.expect
      .when("User or group doesn't exist")
        // there is is no distinction if it is the user or the group or both that do not exist
        .returning(Right(()))
      .when(EndOfFile)
        .returning(Right(()))
    e
  }

  /**
    *
    * @param name name of a group or user
    * @return
    */
  def membership(name: String): Expect[Either[ErrorCase, Set[String]]] = {
    val e = new FluentExpect(s"pts membership -nameorid $name", defaultUnknownError[Set[String]])
    e.expect
      .when(s"User or group doesn't exist")
        .returning(Left(InvalidUserOrGroupName))
      //If $name is a user name or user afs id
      .when(
        """Groups [^ ]+ \(id: \d+\) is a member of:
          |((  [^\n]+\n?)+)""".stripMargin.r)
        .returning { m =>
          val groups = m.group(1).split('\n').map(_.trim).toSet
          Right(groups)
        }
      //If $name is a group name or group afs id
      .when(
        """Members of [^ ]+ \(id: -?\d+\) are:
          |((  [^\n]+\n?)+)""".stripMargin.r)
        .returning { m =>
          val users = m.group(1).split('\n').map(_.trim).toSet
          Right(users)
        }
      .addWhen(unknownError)
    e
  }

  def listGroups(): Expect[Either[ErrorCase, Seq[(String, Int, Int, Int)]]] = {
    val e = new FluentExpect(s"pts listentries -groups", defaultUnknownError[Seq[(String, Int, Int, Int)]])
    e.expect
      .when(
        """Name\s+ID\s+Owner\s+Creator
          |(([^\n]+\n?)+)""".stripMargin.r)
        .returning { m =>
          val groups = m.group(1).split('\n').map { groupString =>
            val Array(name, id, owner, creator) = groupString.trim.split("""\s+""")
            (name, id.toInt, owner.toInt, creator.toInt)
          }.toSeq
          Right(groups)
        }
      .addWhen(unknownError)
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
    val e = new FluentExpect(
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
    val e = new FluentExpect(s"vos remove -server $server -partition $partition -id $name",
      defaultUnknownError[Unit])

    e.expect
      .addWhen(hostNotFound)
      .addWhen(invalidPartition)
      .when("Can't find volume") // makes it idempotent
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
    val e = new FluentExpect(s"vos addsite -server $server -partition $partition -id $name",
      defaultUnknownError[Unit])
    e.expect
      .addWhen(hostNotFound)
      .addWhen(invalidPartition)
      .addWhen(nonExistingVolume)
      .when("RO already exists on partition".r) // makes it idempotent
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
    val e = new FluentExpect(s"vos release -id $name", defaultUnknownError[Unit])
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
    val e = new FluentExpect(s"vos examine -id $name", defaultUnknownError[Unit])
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
    * @param server
    * @return
    */
  def volumeExists(name: String, server: String): Expect[Either[ErrorCase, Unit]] = {
    val e = new FluentExpect(s"vos listvldb -name $name -server $server", defaultUnknownError[Unit])
    e.expect
      .addWhen(nonExistingVolume)
      .when(name)
        .returning(Right(()))
      .addWhen(unknownError)
    e
  }

  /**
    * Creates a backup volume according to "man vos_backup"
    *
    * @param name
    * @return Unit when the volume was created successfully
    */
  def backupVolume(name: String): Expect[Either[ErrorCase, Unit]] = {
    val e = new FluentExpect(s"vos backup -id $name", defaultUnknownError[Unit])
    e.expect
      .addWhen(nonExistingVolume)
        .when("Created backup volume".r)
        .returning(Right(()))
      .addWhen(unknownError)
    e
  }

  //endregion
}