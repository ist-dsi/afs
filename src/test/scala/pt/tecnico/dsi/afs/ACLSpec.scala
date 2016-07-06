package pt.tecnico.dsi.afs

import java.io.File

import org.scalatest.FlatSpec

class ACLSpec extends FlatSpec with TestUtils {
  val afs = new AFS()
  import afs._
  val username = "afsadmin.admin"
  val rwAfsDir = new File("/afs/.example.com" )// dir must be writable
  val invalidDir = new File("/afs/eee")
  val defaultPermissions = (
    Map("system:administrators" -> AllPermissions, "system:anyuser" -> Permission("rl")),
    Map.empty[String, Permission]
  )


  "listACL" should "list ACL successfully" in {
    listACL(rwAfsDir).value shouldBe Right(defaultPermissions)
  }
  it should "list Negative Rights successfully" in {
    val newPermission = username -> (Read + Lookup)
    val newPermissions = (defaultPermissions._1, defaultPermissions._2 + newPermission)

    setACL(rwAfsDir, Map(newPermission), negative = true).rightValueShouldBeUnit()
    listACL(rwAfsDir).rightValue shouldBe newPermissions

    // undo changes to permissions
    setACL(rwAfsDir, Map(username -> NoPermissions), negative = true).rightValueShouldBeUnit()
    listACL(rwAfsDir).rightValue shouldBe defaultPermissions
  }
  it should "return invalidDirectory when the directory does not exist" in {
    listACL(invalidDir).leftValue.shouldBe(InvalidDirectory)
  }

  "setACL" should "set ACL successfully" in {
    val newPermission = username -> (Read + Lookup)
    val newPermissions = (defaultPermissions._1 + newPermission, defaultPermissions._2)

    setACL(rwAfsDir, Map(newPermission)).rightValueShouldBeUnit()
    listACL(rwAfsDir).rightValue shouldBe newPermissions
  }

}
