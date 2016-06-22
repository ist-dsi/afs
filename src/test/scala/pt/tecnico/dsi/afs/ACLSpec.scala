package pt.tecnico.dsi.afs

import java.io.File

import org.scalatest.FlatSpec

/**
  *
  */
class ACLSpec extends FlatSpec with TestUtils {
  val afs = new AFS()
  import afs._
  val username = "afsadmin.admin"
  val userId = 1
  val volumeName = "root.cell"
  val rwAfsDir = new File("/afs/.example.com" )// dir must be writable
  val roAfsDir = new File("/afs/example") // read only dir must set writable first then release
  val invalidDir = new File("/afs/eee")
  val defaultPermissions = (Map(
    "system:administrators" -> AllPermissions,
    "system:anyuser" -> Permission("rl")), Map[String, Permission]())


  "listACL" should "list ACL successfully" in {
    val v = listACL(rwAfsDir).value
    v shouldBe Right(defaultPermissions)
  }
  it should "list Negative Rights successfully" in {
    val newPermission = username -> (Read+Lookup)
    val newPermissions = (defaultPermissions._1, defaultPermissions._2 + newPermission)

    setACL(rwAfsDir, Map(newPermission), negative = true).rightValueShouldBeUnit()
    listACL(rwAfsDir).rightValue shouldBe newPermissions

    // undo changes to permissions
    val newPermission2 = username -> NoPermissions

    setACL(rwAfsDir, Map(newPermission2), negative = true).rightValueShouldBeUnit()
    listACL(rwAfsDir).rightValue shouldBe defaultPermissions
  }
  it should "return invalidDirectory when the directory does not exist" in {
    listACL(invalidDir).leftValue.shouldBe(InvalidDirectory)
  }

  "setACL" should "set ACL successfully" in {
    val newPermission = username -> (Read+Lookup)
    val newPermissions = (defaultPermissions._1 + newPermission, defaultPermissions._2)

    setACL(rwAfsDir, Map(newPermission)).rightValueShouldBeUnit()
    listACL(rwAfsDir).rightValue shouldBe newPermissions
  }

}
