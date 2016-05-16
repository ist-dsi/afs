package pt.tecnico.dsi.afs

import work.martins.simon.expect.fluent.{Expect, ExpectBlock}

object AFSUtils {
  def defaultUnknownError[T]: Either[ErrorCase, T] = Left(UnknownError())

  def obtainTokens(options: String = ""): Expect[Either[ErrorCase, Long]] = {
    val e = new Expect(s"aklog -d $options", defaultUnknownError[Long])
    e.expect
      .when("aklog: ([^\n]+)".r)
        .returning(m => Left(UnknownError(m.group(1))))
      .when("""Id (\d+)""".r)
        .returning(m => Right(m.group(1).toLong))
    e
  }
  def displayTokens(options: String = ""): Expect[Seq[Token]] = {
    val e = new Expect(s"tokens $options", Seq.empty[Token])
    e.expect
      .when("""(?s)(.+?)(?=\s+--End of list--)""".r)
        .returning { m =>
          m.group(1).split('\n').flatMap(Token.fromString).toSeq
        }
    e
  }
  def destroyTokens(): Expect[Unit] = new Expect(s"unlog", ())


  def insufficientPermission[R](expectBlock: ExpectBlock[Either[ErrorCase, R]]) = {
    expectBlock.when("You don't have the required access rights")
      .returning(Left(InsufficientPermissions))
  }
  def invalidDirectory[R](expectBlock: ExpectBlock[Either[ErrorCase, R]]) = {
    expectBlock.when("File '[^']+' doesn't exist".r)
      .returning(Left(InvalidDirectory))
  }
}
