package com.webforj.spring;

import org.springframework.context.ApplicationContext;

/**
 * Holds the {@link ApplicationContext} for the application.
 *
 * @since 25.02
 * @author Hyyan Abo Fakher
 */
public class ContextHolder {
  private static volatile ApplicationContext context;

  private ContextHolder() {
    // Prevent instantiation
  }

  /**
   * Set the {@link ApplicationContext}.
   *
   * @param ctx the {@link ApplicationContext}.
   */
  static void setContext(ApplicationContext ctx) {
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

  /**
   * Clear the {@link ApplicationContext}.
   */
  static void clear() {
    context = null;
  }
}
