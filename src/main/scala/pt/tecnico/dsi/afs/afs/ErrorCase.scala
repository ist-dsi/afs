package pt.tecnico.dsi.afs.afs

trait ErrorCase

case object InvalidDirectory extends ErrorCase
case object InvalidUserOrGroupName extends ErrorCase
case object InvalidUserName extends ErrorCase
case object UnknownUserName extends ErrorCase
case object UnknownUserId extends ErrorCase
case object CouldNotObtainAFSToken extends ErrorCase
case object AFSIdAlreadyTaken extends ErrorCase
case object UnknownError extends ErrorCase
