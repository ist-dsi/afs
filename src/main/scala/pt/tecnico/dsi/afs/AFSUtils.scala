package pt.tecnico.dsi.afs

import work.martins.simon.expect.fluent.ExpectBlock

object AFSUtils {


  def invalidDirectory[R](expectBlock: ExpectBlock[Either[ErrorCase, R]]) = {
    expectBlock.when("File '[^']+' doesn't exist")
      .returning(Left(InvalidDirectory))
  }

}
