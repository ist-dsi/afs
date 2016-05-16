package pt.tecnico.dsi.afs

import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat

case object Expired

object Token {
  def fromString(s: String): Option[Token] = {
    """User's \(AFS ID (\d+)\) tokens for ([^@]+)@([^ ]+) \[(Expired|Expires [^\]])\]""".r
      .findFirstMatchIn(s)
      .map { m =>
        val expiresString = m.group(4)
        val expires: Either[Expired.type, DateTime] = if (expiresString == "Expired") {
          Left(Expired)
        } else {
          val format = DateTimeFormat.forPattern("MMMM dd HH:mm")
          Right(format.parseDateTime(expiresString))
        }
        Token(m.group(1).toLong, m.group(2), m.group(3), expires)
      }
  }
}
case class Token(id: Long, server: String = "afs", cell: String, expires: Either[Expired.type, DateTime])
