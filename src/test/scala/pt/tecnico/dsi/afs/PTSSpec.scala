package pt.tecnico.dsi.afs

import org.scalatest.{AsyncFlatSpec}

class PTSSpec extends AsyncFlatSpec with TestUtils {
  val afs = new AFS()
  import afs._

  val administratorsGroup = "system:administrators"
  val existingUsername1 = "afsadmin.admin"
  val existingId1 = 1
  val group1 = "g1"
  val invalidUsername = "u1:a"
  val username1 = "u1"
  val username2 = "u2"
  val userId1 = 2
  val userId2 = 3
  val userId3 = 4
  val unknownUsername = "u0"
  val unknownGroup = "gx"
  val unknownId = 999


  "getUserId" should "return user id" in {
    getGroupOrUserId(existingUsername1).rightValueShouldIdempotentlyBe(existingId1)
  }
  it should "return error when the user name does not exist" in {
    getGroupOrUserId(unknownUsername).leftValueShouldIdempotentlyBe(UnknownUserName)
  }

  "getUserName" should "return username" in {
    getGroupOrUserName(existingId1).rightValueShouldBe(existingUsername1)
  }
  it should "return error when the user id does not exist" in {
    getGroupOrUserName(unknownId).leftValueShouldBe(UnknownUserOrGroupId)
  }

  "createUser" should "create user successfully" in {
    createUser(username1, userId1).rightValueShouldBeUnit()
  }
  it should "return error when the username in use" in {
    createUser(username1, userId3).leftValueShouldBe(UserNameAlreadyTaken)
  }
  it should "return error when the AFS's id is already in use" in {
    createUser(username2, userId1).leftValueShouldBe(AFSIdAlreadyTaken)
  }
  it should "return error when the name is badly formed" in {
    createUser(invalidUsername, userId2).leftValueShouldBe(BadlyFormedUserName)
  }

  "createGroup" should "create group successfully" in {
    createGroup(group1, username1).rightValue(_ => succeed) // we do not care for the value as least it is a right value
  }
  it should "return error when the group name is already takes" in{
    createGroup(group1, username1).leftValueShouldBe(GroupNameAlreadyTaken)
  }
  it should "return error when the owner does not exist" in{
    createGroup(group1, unknownUsername).leftValueShouldBe(UnknownUserName)
  }

  "deleteUserOrGroup" should "remove user or group successfully" in {
    deleteUserOrGroup(username1).rightValueShouldIdempotentlyBeUnit()
  }

  "addUserToGroup" should "should idemp add user to group successfully" in {
    createUser(username1, userId1).rightValueShouldBeUnit()
    addUserToGroup(username1, group1).rightValueShouldIdempotentlyBeUnit()
  }
  it should "return error when user does not exist" in {
    addUserToGroup(unknownUsername, group1).leftValueShouldBe(InvalidUserOrGroupName)
  }
  it should "return error when group does not exist" in {
    addUserToGroup(username1, unknownGroup).leftValueShouldBe(InvalidUserOrGroupName)
  }


  "removeUserFromGroup" should "remove user from group successfully" in {
    removeUserFromGroup(username1, group1).rightValueShouldIdempotentlyBeUnit()
  }


  "membership" should "list groups of a user" in {
    membership(existingUsername1).rightValueShouldBe(Set(administratorsGroup))
  }
  it should "list users of a group" in {
    membership(administratorsGroup).rightValueShouldBe(Set(existingUsername1))
  }

  "listgroups" should "list groups and their properties" in {
    deleteUserOrGroup(group1).rightValueShouldBeUnit()
    listGroups().rightValueShouldBe(Seq(
      ("system:administrators",   -204, -204, -204),
      ("system:backup",           -205, -204, -204),
      ("system:anyuser",          -101, -204, -204),
      ("system:authuser",         -102, -204, -204),
      ("system:ptsviewers",       -203, -204, -204)
    ))
  }

}
