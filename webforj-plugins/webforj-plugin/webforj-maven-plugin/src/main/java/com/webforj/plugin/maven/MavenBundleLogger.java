package com.webforj.plugin.maven;

import com.webforj.bundle.bun.BundleLogger;
import org.apache.maven.plugin.logging.Log;

/**
 * Reports bundler output through the Maven build log, the channel Maven shows by default.
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
class MavenBundleLogger implements BundleLogger {

  private final Log log;

  MavenBundleLogger(Log log) {
    this.log = log;
  }

  @Override
  public void log(System.Logger.Level level, String message) {
    switch (level) {
      case ERROR -> log.error(message);
      case WARNING -> log.warn(message);
      case DEBUG, TRACE, ALL -> log.debug(message);
      default -> log.info(message);
    }
  }
}
