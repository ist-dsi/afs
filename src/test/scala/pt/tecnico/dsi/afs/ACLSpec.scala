package pt.tecnico.dsi.afs

import java.io.File

import org.scalatest.{AsyncFlatSpec, FlatSpec}

class ACLSpec extends AsyncFlatSpec with TestUtils {
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
    listACL(rwAfsDir).rightValueShouldIdempotentlyBe(defaultPermissions)
  }
  it should "list Negative Rights successfully" in {
    val newPermission = username -> (Read + Lookup)
    val newPermissions = (defaultPermissions._1, defaultPermissions._2 + newPermission)

    setACL(rwAfsDir, Map(newPermission), negative = true).rightValueShouldBeUnit()
    listACL(rwAfsDir).rightValueShouldIdempotentlyBe(newPermissions)

    // undo changes to permissions
    setACL(rwAfsDir, Map(username -> NoPermissions), negative = true).rightValueShouldBeUnit()
    listACL(rwAfsDir).rightValueShouldIdempotentlyBe(defaultPermissions)
  }
  it should "return invalidDirectory when the directory does not exist" in {
    listACL(invalidDir).leftValueShouldIdempotentlyBe(InvalidDirectory)
  }

  "setACL" should "set ACL successfully" in {
    val newPermission = username -> (Read + Lookup)
    val newPermissions = (defaultPermissions._1 + newPermission, defaultPermissions._2)

    setACL(rwAfsDir, Map(newPermission)).rightValueShouldBeUnit()
    listACL(rwAfsDir).rightValueShouldIdempotentlyBe(newPermissions)
  }

}
