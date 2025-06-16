package com.webforj.spring;

import org.springframework.context.ApplicationContext;

/**
 * Holds the {@link ApplicationContext} for the application.
 *
 * @since 25.02
 * @author Hyyan Abo Fakher
 */
public class ContextHolder {
  // is this thread-safe ?
  // adding "volatile" is not enough to make this field thread-safe.sonarqube(java:S3077)
  private static volatile ApplicationContext context;

  /**
   * Set the {@link ApplicationContext}.
   *
   * @param ctx the {@link ApplicationContext}.
   */
  public static void setContext(ApplicationContext ctx) {
    context = ctx;
  }

  /**
   * Get the {@link ApplicationContext}.
   *
   * @return the {@link ApplicationContext}.
   */
  public static ApplicationContext getContext() {
    return context;
  }
}
