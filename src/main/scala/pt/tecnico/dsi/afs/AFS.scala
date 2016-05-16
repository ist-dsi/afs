package pt.tecnico.dsi.afs

import java.io.File
import java.lang

import com.typesafe.config.Config
import com.typesafe.scalalogging.LazyLogging
import pt.tecnico.dsi.afs
import squants.information.InformationConversions._
import squants.information.{Information, Kibibytes}
import work.martins.simon.expect.EndOfFile

import scala.util.matching.Regex.Match
import work.martins.simon.expect.fluent.Expect

class AFS(val settings: Settings = new Settings()) extends LazyLogging {
  def this(config: Config) = this(new Settings(config))
  import settings._

  //region <FS commands>
  /**
    *
    * @param directory
    * @return
    */
  def listquota(directory: File): Expect[Either[ErrorCase, (String, Information, Information)]] = {
    val dir = directory.getPath
    val command = s"fs listquota $dir"
    val e = new Expect[Either[ErrorCase, (String, Information, Information)]](command, Left(afs.UnknownError))
    e.expect
      .when(s"File '$dir' doesn't exist")
        .returning(Left(InvalidDirectory))
        //Note that Quota might be "no limit" but since we do not allow to set a quota to no limit we don't handle this case
      .when("""Volume Name\s+Quota\s+Used\s+%Used\s+Partition
        |([^\s]+)\s+(\d+)\s+(\d+)""".stripMargin.r)
        .returning { m: Match =>
          //Quota and Used are in kilobytes
          Right((m.group(1), Kibibytes(m.group(2).toInt), Kibibytes(m.group(3).toInt)))
        }
      .when(EndOfFile)
        .returning(Left(afs.UnknownError))
    e
  }
  /**
    *
    * @param directory
    * @param quota
    * @return
    */
  def setQuota(directory: File, quota: Information): Expect[Either[ErrorCase, Unit]] = {
    require(quota >= 0.kibibytes, "new quota must be positive")
    val dir = directory.getPath
    val command = s"fs setquota -path $dir -max ${quota.toKibibytes.toLong}"
    val e = new Expect[Either[ErrorCase, Unit]](command, Left(afs.UnknownError))
    e.expect
      .when(s"File '$dir' doesn't exist")
        .returning(Left(InvalidDirectory))
      .when(".+".r)
        .returning(Left(afs.UnknownError))
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
    val command = s"fs lsmount $dir"
    val e = new Expect[Either[ErrorCase, String]](command, Left(afs.UnknownError))
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
  /**
    *
    * @param directory
    * @param volume
    * @return
    */
  def makeMount(directory: File, volume: String): Expect[Either[ErrorCase, Boolean]] = {
    val dir = directory.getPath
    val e = new Expect[Either[ErrorCase, Boolean]](s"fs mkmount -dir $dir -vol $volume", Left(afs.UnknownError))
    e.expect
      .when(s"File '$dir' doesn't exist")
        .returning(Left(InvalidDirectory))
      .when("mount points must be created within the AFS file system")
        .returning(Left(InvalidDirectory))
      .when(EndOfFile)
        .returning(Right(true))
    e
  }
  /**
    *
    * @param directory
    * @return
    */
  def removeMount(directory: File): Expect[Either[ErrorCase, Boolean]] = {
    val dir = directory.getPath
    val e = new Expect[Either[ErrorCase, Boolean]](s"fs rmmount -dir $dir", Left(afs.UnknownError))
    e.expect
      .when(s"File '$dir' doesn't exist")
        .returning(Left(InvalidDirectory))
      .when(EndOfFile)
        .returning(Right(true))
    e
  }

  /**
    *
    * @param directory
    * @return
    */
  def listACL(directory: File): Expect[Either[ErrorCase, Map[String, Permission]]] = {
    val dir = directory.getPath
    val e = new Expect[Either[ErrorCase, Map[String, Permission]]](s"fs listacl -dir $dir", Left(afs.UnknownError))
    e.expect
      .when(s"File '$dir' doesn't exist")
        .returning(Left(InvalidDirectory))
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
  /**
    *
    * @param directory
    * @param acls
    * @return
    */
  def setACL(directory: File, acls: Map[String, Permission]): Expect[Either[ErrorCase, Boolean]] = {
    val aclsString = acls.map(t => t._1 + " " + t._2.acl).mkString(" ")
    val dir = directory.getPath
    val e = new Expect[Either[ErrorCase, Boolean]](s"fs setacl -dir $dir -acl $aclsString", Left(afs.UnknownError))
    e.expect
      .when(s"File '$dir' doesn't exist")
        .returning(Left(InvalidDirectory))
      .when("Tried to add non-existent user to access control list")
        .returning(Left(InvalidUserOrGroupName))
      .when(EndOfFile)
        .returning(Right(true))
    e
  }

  /**
    *
    * @return
    */
  def checkVolumes(): Expect[Either[ErrorCase, Boolean]] = {
    val e = new Expect[Either[ErrorCase, Boolean]](s"fs checkvolumes", Left(afs.UnknownError))
    e.expect("All volumeID/name mappings checked.")
      .returning(Right(true))
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
    val cmd = s"pts examine -nameorid $username"
    val e = new Expect[Either[ErrorCase, String]](cmd, Left(afs.UnknownError))
    e.expect
      .when("""id: (\d+)""".r)
        .returning { m =>
          Right(m.group(1))
        }
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
    val cmd = s"pts examine -nameorid $id"
    val e = new Expect[Either[ErrorCase, String]](cmd, Left(afs.UnknownError))
    e.expect
      .when("""Name: ([\w\.]+),""".r)
        .returning { m =>
          Right(m.group(1))
        }
      .when("""User or group doesn't exist""".r)
        .returning(Left(UnknownUserId))
    e
  }

  /**
    *
    * @param name
    * @param afsId
    * @return
    */
  def createUser(name: String, afsId: Int): Expect[Either[ErrorCase, Boolean]] = {
    val e = new Expect[Either[ErrorCase, Boolean]](s"pts createuser -name $name -id $afsId", Left(afs.UnknownError))
    e.expect
      .when(s"Entry for id already exists ; unable to create user '$name' with id '$afsId'")
        .returning(Left(AFSIdAlreadyTaken))
      .when(s"Badly formed name (group prefix doesn't match owner?) ; unable to create user '$name' with id '$afsId'")
        .returning(Left(InvalidUserName))
      .when(s"User '$name' has id '$afsId'")
        .returning(Right(true))
    e
  }

  /**
    *
    * @param name
    * @param owner
    * @return
    */
  def createGroup(name: String, owner: String): Expect[Either[ErrorCase, Int]] = {
    val e = new Expect[Either[ErrorCase, Int]](s"pts creategroup -name $name -owner $owner", Left(afs.UnknownError))
    e.expect
      .when(s"""User or group doesn't exist ; unable to create group $name with id -?\d+ owned by $owner'""".r)
        .returning(Left(InvalidUserName))
      .when(s"""group $name has id (-\d+)""".r)
        .returning { m: Match => Right(m.group(1).toInt) }
    e
  }

  /**
    *
    * @param name
    * @return
    */
  def deleteUserOrGroup(name: String): Expect[Either[ErrorCase, Boolean]] = {
    //Should we also call fs cleanacl?
    val e = new Expect[Either[ErrorCase, Boolean]](s"pts delete -nameorid $name", Left(afs.UnknownError))
    e.expect
      .when(s"User or group doesn't exist so couldn't look up id for $name")
        .returning(Left(InvalidUserOrGroupName))
      .when(EndOfFile)
        .returning(Right(true))
    e
  }

  /**
    *
    * @param name
    * @param group
    * @return
    */
  def addUserToGroup(name: String, group: String): Expect[Either[ErrorCase, Boolean]] = {
    val e = new Expect[Either[ErrorCase, Boolean]](s"pts adduser -user $name -group $group", Left(afs.UnknownError))
    e.expect
      .when(s"User or group doesn't exist ; unable to add user $name to group $group")
        .returning(Left(InvalidUserOrGroupName))
      .when(EndOfFile)
        .returning(Right(true))
    e
  }

  /**
    *
    * @param name
    * @param group
    * @return
    */
  def removeUserFromGroup(name: String, group: String): Expect[Either[ErrorCase, Boolean]] = {
    val e = new Expect[Either[ErrorCase, Boolean]](s"pts removeuser -user $name -group $group", Left(afs.UnknownError))
    e.expect
      .when(s"User or group doesn't exist ; unable to remove user $name to group $group")
        .returning(Left(InvalidUserOrGroupName))
      .when(EndOfFile)
        .returning(Right(true))
    e
  }

  /**
    *
    * @param name
    * @return
    */
  def membership(name: String): Expect[Either[ErrorCase, Set[String]]] = {
    val e = new Expect[Either[ErrorCase, Set[String]]](s"pts membership -nameOrId $name", Left(afs.UnknownError))
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

  /**
    *
    * @return
    */
  def listGroups(): Expect[Either[ErrorCase, Seq[(String, Int, Int, Int)]]] = {
    val e = new Expect[Either[ErrorCase, Seq[(String, Int, Int, Int)]]](s"pts listentries -groups", Left(afs.UnknownError))
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
  /**
    *
    * @param name
    * @return
    */
  def backupVolume(name: String): Expect[Either[ErrorCase, Boolean]] = {
    val e = new Expect[Either[ErrorCase, Boolean]](s"vos backup -id $name", Left(afs.UnknownError))
    e.expect
      .when(s"Created backup volume for I$name")
        .returning(Right(true))
    e
  }

  /**
    *
    * @param server
    * @param partition
    * @param name
    * @param maxQuota
    * @return
    */
  def createVolume(server: String, partition: String, name: String, maxQuota: Information): Expect[Either[ErrorCase, Boolean]] = {
    val e = new Expect[Either[ErrorCase, Boolean]](
      s"vos create -server $server -partition $partition" +
        s" -name $name -maxquota ${maxQuota.toKibibytes.toLong}K", Left(afs.UnknownError))
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
        .returning(Right(true))
      .when(EndOfFile)
        .returning(Left(UnknownError))
    e
  }

  /**
    *
    * @param name
    * @return
    */
  def removeVolume(name: String): Expect[Either[ErrorCase, Boolean]] = {
    val e = new Expect[Either[ErrorCase, Boolean]](s"vos remove -id $name", Left(afs.UnknownError))
    e.expect
      .when(s"")
        .returning(Right(true))
    e
  }

  /**
    *
    * @param name
    * @return
    */
  def examineVolume(name: String): Expect[Either[ErrorCase, Boolean]] = {
    val e = new Expect[Either[ErrorCase, Boolean]](s"vos examine -id $name", Left(afs.UnknownError))
    e.expect
      .when(s"")
        .returning(Right(true))
    e
  }

  /**
    *
    * @param name
    * @return
    */
  def releaseVolume(name: String): Expect[Either[ErrorCase, Boolean]] = {
    val e = new Expect[Either[ErrorCase, Boolean]](s"vos release -id $name", Left(UnknownError))
    e.expect
      .when(EndOfFile)
        .returning(Right(true))
    e
  }
  //endregion
}