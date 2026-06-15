package com.webforj.plugin.gradle;

import com.webforj.bundle.bun.BundleLogger;
import org.gradle.api.logging.Logger;

/**
 * Reports bundler output through the Gradle logger.
 *
 * <p>
 * Gradle hides the info log unless the build runs with the info flag, so progress and results are
 * reported at the lifecycle level Gradle shows by default. The raw build output stays at info,
 * where a developer reaches for it only when something looks wrong.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
class GradleBundleLogger implements BundleLogger {

  private final Logger log;

  GradleBundleLogger(Logger log) {
    this.log = log;
  }

  @Override
  public void log(System.Logger.Level level, String message) {
    switch (level) {
      case ERROR -> log.error(message);
      case WARNING -> log.warn(message);
      case DEBUG, TRACE, ALL -> log.info(message);
      default -> log.lifecycle(message);
    }
  }
}
