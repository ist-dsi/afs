package pt.tecnico.dsi.afs

trait ErrorCase

// PTS errors
case object InvalidDirectory extends ErrorCase
case object InvalidVolume extends ErrorCase
case object InvalidUserOrGroupName extends ErrorCase
case object BadlyFormedUserName extends ErrorCase
case object AFSIdAlreadyTaken extends ErrorCase
case object UserNameAlreadyTaken extends ErrorCase
case object GroupNameAlreadyTaken extends ErrorCase
case object UnknownUserName extends ErrorCase
case object UnknownUserOrGroupId extends ErrorCase
case object CouldNotObtainAFSToken extends ErrorCase
// set acl
case object CanNotChangeReadOnlyVolume extends ErrorCase
// mount
case object HostNotFound extends ErrorCase
case object InvalidPartition extends ErrorCase
case object InvalidVolumeName extends ErrorCase
case object InvalidVolumeQuota extends ErrorCase
case object InsufficientPermissions extends ErrorCase
case object NotAMountPoint extends ErrorCase
case object FileAlreadyExists extends ErrorCase
// vos
case object ReadOnlyPartitionAlreadyExists extends ErrorCase
case object NonExistingVolume extends ErrorCase
case object VLDBAlreadyLocked extends ErrorCase

object UnknownError {
  def apply(message: String): UnknownError = new UnknownError(Some(new Exception(message)))
}
case class UnknownError(cause: Option[Throwable] = None) extends ErrorCase