package pt.tecnico.dsi.afs

import java.io.File

import com.typesafe.config.Config
import com.typesafe.scalalogging.LazyLogging
import squants.information.InformationConversions._
import squants.information.Information
import work.martins.simon.expect.EndOfFile

import scala.util.matching.Regex.Match
import work.martins.simon.expect.fluent.Expect
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
      .when("""Volume Name\s+Quota\s+Used\s+%Used\s+Partition
        |([^\s]+)\s+(\d+|no limit)\s+(\d+)""".stripMargin.r)
        .returning { m: Match =>
          val quotaDouble = if (m.group(2) == "no limit") Double.PositiveInfinity else m.group(2).toDouble
          //Quota and Used are in kibibytes contrary to what is stated in the documentation
          Right(Quota(m.group(1), quotaDouble.kibibytes, m.group(3).toDouble.kibibytes))
        }
    e
  }
  def setQuota(directory: File, quota: Information): Expect[Either[ErrorCase, Unit]] = {
    require(quota >= 0.kibibytes, "new quota must be positive")
    val dir = directory.getPath
    val e = new Expect(s"fs setquota -path $dir -max ${quota.toKibibytes.toLong}K", defaultUnknownError[Unit])
    e.expect
      .addWhen(insufficientPermission)
      .addWhen(invalidDirectory)
      .when(".+".r)
        .returning(Left(UnknownError()))
      .when(EndOfFile)
        .returning(Right(()))
    e
  }

  /**
    * For more details see "man fs_lsmount"
    * @param directory
    * @return
    */
  def listMount(directory: File): Expect[Either[ErrorCase, String]] = {
    val dir = directory.getPath
    val e = new Expect(s"fs lsmount $dir", defaultUnknownError[String])
    e.expect
      .when(s"'$dir' is not a mount point.")
        .returning(Left(InvalidMountPoint))
      .when(s"'$dir' doesn't exist")
        .returning(Left(InvalidDirectory))
      .when(s"'$dir' is a mount point for volume '([^']+)'".r)
        .returning { m: Match =>
          Right(m.group(1))
        }
    e
  }
  def makeMount(directory: File, volume: String): Expect[Either[ErrorCase, Unit]] = {
    val dir = directory.getPath
    val e = new Expect(s"fs mkmount -dir $dir -vol $volume", defaultUnknownError[Unit])
    e.expect
      .addWhen(invalidDirectory)
      .when("mount points must be created within the AFS file system")
        .returning(Left(InvalidDirectory))
      .when(EndOfFile)
        .returning(Right(()))
    e
  }
  def removeMount(directory: File): Expect[Either[ErrorCase, Unit]] = {
    val dir = directory.getPath
    val e = new Expect(s"fs rmmount -dir $dir", defaultUnknownError[Unit])
    e.expect
      .addWhen(invalidDirectory)
      .when(EndOfFile)
        .returning(Right(()))
    e
  }

  def listACL(directory: File): Expect[Either[ErrorCase, Map[String, Permission]]] = {
    val dir = directory.getPath
    val e = new Expect(s"fs listacl -dir $dir", defaultUnknownError[Map[String, Permission]])
    e.expect
      .addWhen(invalidDirectory)
      .when(s"""Access list for $dir is
               |Normal rights:
               |(  [\w:.]+ \w\n?)+""".stripMargin.r)
        .returning { m: Match =>
          val permissions = m.group(0).split('\n').map { acl =>
            val Array(userOrGroup, permissionAcl) = acl.trim.split(" ")
            (userOrGroup, Permission(permissionAcl))
          }.toMap
          Right(permissions)
        }
    e
  }
  def setACL(directory: File, acls: Map[String, Permission]): Expect[Either[ErrorCase, Unit]] = {
    val aclsString = acls.map(t => t._1 + " " + t._2.acl).mkString(" ")
    val e = new Expect(s"fs setacl -dir ${directory.getPath} -acl $aclsString", defaultUnknownError[Unit])
    e.expect
      .addWhen(invalidDirectory)
      .when("Tried to add non-existent user to access control list")
        .returning(Left(InvalidUserOrGroupName))
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
    val e = new Expect(s"pts membership -nameOrId $name", defaultUnknownError[Set[String]])
    e.expect
      .when(s"User or group doesn't exist so couldn't look up id for $name")
        .returning(Left(InvalidUserOrGroupName))
      //If $name is a user name or user afs id
      .when(s"""Groups [^ ]+ id: \d+ is a member of:
               |(  [^\n]+\n?)+""".stripMargin.r)
        .returning { m: Match =>
          val groups = m.group(0).split('\n').map(_.trim).toSet
          Right(groups)
        }
      //If $name is a group name or group afs id
      .when(s"""Members of group [^ ]+ id: -\d+ are:
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
      .when(s"""Name\tID\tOwner\tCreator
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
  /** */
  def backupVolume(name: String): Expect[Either[ErrorCase, Unit]] = {
    val e = new Expect(s"vos backup -id $name", defaultUnknownError[Unit])
    e.expect
      .when(s"Created backup volume for I$name")
        .returning(Right(()))
    e
  }
  def createVolume(server: String, partition: String, name: String, maxQuota: Information): Expect[Either[ErrorCase, Unit]] = {
    val e = new Expect(
      s"vos create -server $server -partition $partition -name $name -maxquota ${maxQuota.toKibibytes.toLong}K",
      defaultUnknownError[Unit])
    e.expect
      .when(s"host '$server' not found")
        .returning(Left(HostNotFound))
      .when(s"partition $partition does not exist on server")
        .returning(Left(InvalidPartition))
      .when("already exists")
        .returning(Left(InvalidVolumeName))
      .when("bad integer specified for quotal")
        .returning(Left(InvalidVolumeQuota))
      .when(s"""Volume \\d+ created on partition $partition of $server""".r)
        .returning(Right(()))
      .when(EndOfFile)
        .returning(Left(UnknownError))
    e
  }
  def removeVolume(name: String): Expect[Either[ErrorCase, Unit]] = {
    val e = new Expect(s"vos remove -id $name", defaultUnknownError[Unit])
    e.expect
      .when(s"")
        .returning(Right(()))
    e
  }
  def examineVolume(name: String): Expect[Either[ErrorCase, Unit]] = {
    val e = new Expect(s"vos examine -id $name", defaultUnknownError[Unit])
    e.expect
      .when(s"")
        .returning(Right(()))
    e
  }
  def releaseVolume(name: String): Expect[Either[ErrorCase, Unit]] = {
    val e = new Expect(s"vos release -id $name", defaultUnknownError[Unit])
    e.expect
      .when(EndOfFile)
        .returning(Right(()))
    e
  }
  //endregion
}