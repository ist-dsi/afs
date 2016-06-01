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
    "system:anyuser" -> Permission("rl")), Map())


  "listACL" should "list ACL successfully" in {
    val v = listACL(rwAfsDir).value
    logger.info(s"Expect returned $v")
    v shouldBe Right(defaultPermissions)
  }
  it should "return invalidDirectory" in {
    listACL(invalidDir).leftValue.shouldBe(InvalidDirectory)
  }

  "setACL" should "set ACL successfully" in {
    val newPermission = username -> Read

    setACL(rwAfsDir, Map(newPermission)).rightValueShouldBeUnit()
    val newMap = (defaultPermissions._1 + newPermission, defaultPermissions._2)

    listACL(rwAfsDir).rightValue shouldBe newMap
  }

}
