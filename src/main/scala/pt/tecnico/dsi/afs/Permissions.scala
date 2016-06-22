package pt.tecnico.dsi.afs

import scala.collection.immutable.Set

object Permission {
  /**
    * Constructs a Permission from the string representation `acl`.
    *
    * @param acl the String representation of the permission.
    * @return the Permission corresponding to the 'acl' String.
    *         If the 'acl' contains invalid permissions they will be ignored.
    *         When every character of the string is an invalid permission, `NoPermissions` will be returned.
    */
  protected[afs] def apply(acl: String): Permission = acl match {
    case s if s == AllPermissions.acl => AllPermissions
    case s if s == NoPermissions.acl => NoPermissions
    case s if s == FullWrite.acl => FullWrite
    case s if s == FullRead.acl => FullRead
    case s =>
      val aclCharToPermission: Map[Char, SinglePermission] = AllPermissions.permissions.map(p => (p.acl.head, p)).toMap
      //If s contains a char that does not correspond to a permission then that char will be ignored.
      //Which in the extreme case that every char does not correspond to a permission the NoPermission will be returned.
      //Because the result will be an empty.
      val permissions = s.flatMap(c => aclCharToPermission.get(c)).toSet
      if (permissions.size == 1) {
        permissions.head
      } else {
        MultiplePermissions(permissions)
      }
  }
}

sealed trait Permission extends Serializable {
  protected[afs] def acl: String

  def +(permission: SinglePermission): MultiplePermissions = this match {
    case s: SinglePermission =>
      MultiplePermissions(Set(s, permission))
    case (m: MultiplePermissions) =>
      MultiplePermissions(m.permissions + permission)
  }

  def -(permission: SinglePermission): MultiplePermissions = this match {
    case s: SinglePermission =>
      if (s == permission) {
        MultiplePermissions(Set.empty)
      } else {
        MultiplePermissions(Set(s))
      }
    case (m: MultiplePermissions) =>
      MultiplePermissions(m.permissions - permission)
  }
}

sealed abstract class SinglePermission(protected[afs] val acl: String) extends Permission

case object Administer extends SinglePermission("a")

case object Delete extends SinglePermission("d")

case object Insert extends SinglePermission("i")

case object Lock extends SinglePermission("k")

case object Lookup extends SinglePermission("l")

case object Read extends SinglePermission("r")

case object Write extends SinglePermission("w")

object MultiplePermissions {
  def apply(permissions: Set[SinglePermission]): MultiplePermissions = {
    if (permissions.isEmpty) {
      NoPermissions
    } else if (permissions == AllPermissions.permissions) {
      AllPermissions
    } else if (permissions == FullWrite.permissions) {
      FullWrite
    } else if (permissions == FullRead.permissions) {
      FullRead
    } else {
      new MultiplePermissions(permissions)
    }
  }
}

sealed class MultiplePermissions(val permissions: Set[SinglePermission]) extends Permission {
  protected[afs] val acl = permissions.map(_.acl).mkString

  override def toString = acl

  override def equals(other: Any): Boolean = other match {
    case that: MultiplePermissions => permissions == that.permissions
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(permissions)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

case object AllPermissions extends MultiplePermissions(Set(Administer, Delete, Insert, Lock, Lookup, Read, Write)) {
  override protected[afs] val acl: String = "all"
}

/**
  * This permission is only present in a map of permissions
  */
case object NoPermissions extends MultiplePermissions(Set.empty) {
  override protected[afs] val acl: String = "none"
}

case object FullWrite extends MultiplePermissions(Set(Delete, Insert, Lock, Lookup, Read, Write)) {
  override protected[afs] val acl: String = "write"
}

case object FullRead extends MultiplePermissions(Set(Read, Lookup)) {
  override protected[afs] val acl: String = "read"
}