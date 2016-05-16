package pt.tecnico.dsi.afs

import org.scalatest.FlatSpec
import pt.tecnico.dsi.afs.AFSUtils._
import pt.tecnico.dsi.kadmin.KadminUtils

class TokensSpec extends FlatSpec with TestUtils {

  //TODO: read these values from the afs.env file in the docker-afs folder
  val adminPrincipal = "afsadmin/admin"
  val adminPassword = "bRp1rr4zwZsxYhVtEXVM0pJfGqpuFYCsto5kl2vSNng9f2wiUSilJhr2Fcqh"

  "obtainTokens" should "fail when there are no Kerberos tickets " in {
    KadminUtils.destroyTickets().value.shouldBe(())
    obtainTokens().leftValue shouldBe a [UnknownError]
    KadminUtils.obtainTGT("", adminPrincipal, password = Some(adminPassword)).rightValueShouldBeUnit()
  }
  it should "succeed when there is a Kerberos ticket available" in {
    KadminUtils.obtainTGT("", adminPrincipal, password = Some(adminPassword)).rightValueShouldBeUnit()
    val afsID = obtainTokens().rightValue
    displayTokens().value.exists(_.id == afsID) shouldBe true
  }

  "displayTokens" should "return empty seq if there are no tokens" in {
    destroyTokens().value.shouldBe(())
    displayTokens().value shouldBe empty
  }
  it should "return at least one token if there are tokens available" in {
    destroyTokens().value.shouldBe(())
    obtainTokens().rightValue shouldBe a [scala.Long]
    displayTokens().value should not be empty
  }

  "destroyTokens" should "always succeed" in {
    destroyTokens().value.shouldBe(())
  }
}
