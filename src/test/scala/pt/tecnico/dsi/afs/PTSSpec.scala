package pt.tecnico.dsi.afs

import org.scalatest.FlatSpec

class PTSSpec extends FlatSpec with TestUtils {
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
    getGroupOrUserId(existingUsername1).rightValue shouldBe existingId1
  }
  it should "return error when the user name does not exist" in {
    getGroupOrUserId(unknownUsername).leftValue shouldBe UnknownUserName
  }

  "getUserName" should "return username" in {
    getGroupOrUserName(existingId1).rightValue shouldBe existingUsername1
  }
  it should "return error when the user id does not exist" in {
    getGroupOrUserName(unknownId).leftValue shouldBe UnknownUserOrGroupId
  }

  "createUser" should "create user successfully" in {
    createUser(username1, userId1).rightValueShouldBeUnit()
  }
  it should "return error when the username in use" in {
    createUser(username1, userId3).leftValue shouldBe UserNameAlreadyTaken
  }
  it should "return error when the AFS's id is already in use" in {
    createUser(username2, userId1).leftValue shouldBe AFSIdAlreadyTaken
  }
  it should "return error when the name is badly formed" in {
    createUser(invalidUsername, userId2).leftValue shouldBe BadlyFormedUserName
  }

  "createGroup" should "create group successfully" in {
    createGroup(group1, username1).rightValue // we do not care for the value as least it is a right value
  }
  it should "return error when the group name is already takes" in{
    createGroup(group1, username1).leftValue shouldBe GroupNameAlreadyTaken
  }
  it should "return error when the owner does not exist" in{
    createGroup(group1, unknownUsername).leftValue shouldBe UnknownUserName
  }

  "deleteUserOrGroup" should "remove user or group successfully" in {
    deleteUserOrGroup(username1).rightValueShouldIdempotentlyBeUnit()
  }

  "addUserToGroup" should "should idemp add user to group successfully" in {
    createUser(username1, userId1).rightValueShouldBeUnit()
    addUserToGroup(username1, group1).rightValueShouldIdempotentlyBeUnit()
  }
  it should "return error when user does not exist" in {
    addUserToGroup(unknownUsername, group1).leftValue.shouldBe(InvalidUserOrGroupName)
  }
  it should "return error when group does not exist" in {
    addUserToGroup(username1, unknownGroup).leftValue.shouldBe(InvalidUserOrGroupName)
  }


  "removeUserFromGroup" should "remove user from group successfully" in {
    removeUserFromGroup(username1, group1).rightValueShouldIdempotentlyBeUnit()
  }


  "membership" should "list groups of a user" in {
    membership(existingUsername1).rightValue should contain only administratorsGroup
  }
  it should "list users of a group" in {
    membership(administratorsGroup).rightValue should contain only existingUsername1
  }

  "listgroups" should "list groups and their properties" in {
    deleteUserOrGroup(group1).rightValueShouldBeUnit()
    listGroups().rightValue should contain theSameElementsInOrderAs Seq(
      ("system:administrators",   -204, -204, -204),
      ("system:backup",           -205, -204, -204),
      ("system:anyuser",          -101, -204, -204),
      ("system:authuser",         -102, -204, -204),
      ("system:ptsviewers",       -203, -204, -204)
    )
  }



}
