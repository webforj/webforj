package com.webforj.devtools.craftforj.router.model;

/**
 * Route alias from @RouteAlias annotation.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class RouteAliasInfo {

  private String path;
  private int priority;

  /**
   * Gets the alias path.
   *
   * @return the path
   */
  public String getPath() {
    return path;
  }

  /**
   * Sets the alias path.
   *
   * @param path the path
   */
  public void setPath(String path) {
    this.path = path;
  }

  /**
   * Gets the alias priority.
   *
   * @return the priority
   */
  public int getPriority() {
    return priority;
  }

  /**
   * Sets the alias priority.
   *
   * @param priority the priority
   */
  public void setPriority(int priority) {
    this.priority = priority;
  }
}
