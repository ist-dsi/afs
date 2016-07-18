package pt.tecnico.dsi.afs

import java.io.File

import com.typesafe.config.{Config, ConfigFactory, ConfigValueType}
import work.martins.simon.expect.{Settings => ScalaExpectSettings}
import work.martins.simon.expect.StringUtils.splitBySpaces

import scala.collection.JavaConverters._

/**
  * This class holds all the settings that parameterize AFS.
  *
  * By default these settings are read from the Config obtained with `ConfigFactory.load()`.
  *
  * You can change the settings in multiple ways:
  *
  *  - Change them in the default configuration file (e.g. application.conf)
  *  - Pass a different config holding your configurations: {{{
  *       new Settings(yourConfig)
  *     }}}
  *     However it will be more succinct to pass your config directly to AFS: {{{
  *       new AFS(yourConfig)
  *     }}}
  *  - Extend this class overriding the settings you want to redefine {{{
  *      object YourSettings extends Settings() {
  *        override val realm: String = "YOUR.DOMAIN.TLD"
  *        override val keytabsLocation: String = "/var/local/keytabs"
  *        override val commandWithAuthentication: String = s"""ssh user@server:port "kadmin -p \$authenticatingPrincipal""""
  *      }
  *      new AFS(YourSettings)
  *    }}}
  *
  * @param config
  */
class Settings(config: Config = ConfigFactory.load()) {
  val afsConfig: Config = {
    val reference = ConfigFactory.defaultReference()
    val finalConfig = config.withFallback(reference)
    finalConfig.checkValid(reference, "afs")
    finalConfig.getConfig("afs")
  }
  import afsConfig._

  val cell = getString("cell")
  val cacheDir = new File(getString("cache-dir"))

  val scalaExpectSettings = {
    val path = "scala-expect"
    if (afsConfig.hasPath(path)) {
      val c = if (config.hasPath(path)) {
        afsConfig.getConfig(path).withFallback(config.getConfig(path))
      } else {
        afsConfig.getConfig(path)
      }
      new ScalaExpectSettings(c.atPath(path))
    } else if (config.hasPath(path)) {
      new ScalaExpectSettings(config.getConfig(path).atPath(path))
    } else {
      new ScalaExpectSettings()
    }
  }

  override def toString: String = afsConfig.root.render
}

