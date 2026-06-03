package com.webforj.spring.devtools;

import org.springframework.boot.Banner;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.SpringApplicationRunListener;
import org.springframework.boot.bootstrap.ConfigurableBootstrapContext;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.core.Ordered;

/**
 * Prints the application banner once and suppresses it on every Spring DevTools restart.
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public final class RestartBannerSuppressor implements SpringApplicationRunListener, Ordered {

  /**
   * JVM wide marker that the banner has been printed. It is a system property because it must
   * survive a restart, where a static field would not since it lives on the restart classloader
   * that DevTools discards.
   */
  private static final String SHOWN_PROPERTY = "webforj.devtools.banner-shown";

  private final SpringApplication application;

  /**
   * Creates the run listener. This constructor is used by Spring Boot.
   *
   * @param application the application being started
   * @param args the application arguments
   */
  public RestartBannerSuppressor(SpringApplication application, String[] args) {
    this.application = application;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int getOrder() {
    return Ordered.HIGHEST_PRECEDENCE;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void starting(ConfigurableBootstrapContext bootstrapContext) {
    if (System.getProperty(SHOWN_PROPERTY) != null) {
      application.setBannerMode(Banner.Mode.OFF);
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void contextPrepared(ConfigurableApplicationContext context) {
    // Runs after the banner is printed and only on the restarted run, so the pre relaunch pass on
    // the base classloader never marks the banner as shown.
    System.setProperty(SHOWN_PROPERTY, "true");
  }
}
