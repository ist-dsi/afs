package pt.tecnico.dsi.afs

import work.martins.simon.expect.EndOfFile
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
  def unknownError[R](expectBlock: ExpectBlock[Either[ErrorCase, R]]) = {
    expectBlock.when("(.+)$".r)
      .returning(m => Left(UnknownError(m.group(1))))
  }

  def notAMountPoint[R](expectBlock: ExpectBlock[Either[ErrorCase, R]]) = {
    expectBlock.when(s"is not a mount point.")
      .returning(Left(NotAMountPoint))
  }

  def successOnEndOfFile(expectBlock: ExpectBlock[Either[ErrorCase, Unit]]) = {
    expectBlock.when(EndOfFile)
      .returning(Right(()))
  }

  def hostNotFound[R](expectBlock: ExpectBlock[Either[ErrorCase, R]]) = {
    expectBlock.when("not found in host table".r)
      .returning(Left(HostNotFound))
  }

  def invalidPartition[R](expectBlock: ExpectBlock[Either[ErrorCase, R]]) = {
    expectBlock.when("partition .+ does not exist".r)
      .returning(Left(InvalidPartition))
  }

  def nonExistingVolume[R](expectBlock: ExpectBlock[Either[ErrorCase, R]]) = {
    expectBlock.when("VLDB: no such entry".r)
      .returning(Left(NonExistingVolume))
  }



}
