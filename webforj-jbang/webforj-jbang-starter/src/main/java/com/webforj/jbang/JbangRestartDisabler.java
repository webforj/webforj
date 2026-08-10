package com.webforj.jbang;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.SpringApplicationRunListener;

/**
 * Turns the Spring DevTools restart machinery off for JBang scripts.
 *
 * <p>
 * A script runs from a jar that JBang rebuilds on every launch, so there is no development
 * classpath for a restart classloader to manage. With restart off, the rest of the development
 * tools on the script classpath keep working as they do in any packaged application.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public final class JbangRestartDisabler implements SpringApplicationRunListener {

  static final String RESTART_ENABLED_PROPERTY = "spring.devtools.restart.enabled";

  /**
   * Creates the run listener.
   *
   * @param application the application being started
   * @param args the application arguments
   */
  public JbangRestartDisabler(SpringApplication application, String[] args) {
    System.setProperty(RESTART_ENABLED_PROPERTY, "false");
  }
}
