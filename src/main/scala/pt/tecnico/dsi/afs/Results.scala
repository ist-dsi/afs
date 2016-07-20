package pt.tecnico.dsi.afs

import squants.information.Information

case class Quota(volume: String, quota: Information, used: Information)
