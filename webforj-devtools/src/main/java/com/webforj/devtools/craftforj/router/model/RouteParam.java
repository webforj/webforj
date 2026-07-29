package com.webforj.devtools.craftforj.router.model;

/**
 * Route parameter from a route pattern.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class RouteParam {

  private String name;
  private String constraint;
  private boolean optional;
  private boolean wildcard;

  /**
   * Gets the parameter name.
   *
   * @return the name
   */
  public String getName() {
    return name;
  }

  /**
   * Sets the parameter name.
   *
   * @param name the name
   */
  public void setName(String name) {
    this.name = name;
  }

  /**
   * Gets the regex constraint.
   *
   * @return the constraint or null
   */
  public String getConstraint() {
    return constraint;
  }

  /**
   * Sets the regex constraint.
   *
   * @param constraint the constraint
   */
  public void setConstraint(String constraint) {
    this.constraint = constraint;
  }

  /**
   * Checks if the parameter is optional.
   *
   * @return true if optional
   */
  public boolean isOptional() {
    return optional;
  }

  /**
   * Sets whether the parameter is optional.
   *
   * @param optional true if optional
   */
  public void setOptional(boolean optional) {
    this.optional = optional;
  }

  /**
   * Checks if the parameter is a wildcard.
   *
   * @return true if wildcard
   */
  public boolean isWildcard() {
    return wildcard;
  }

  /**
   * Sets whether the parameter is a wildcard.
   *
   * @param wildcard true if wildcard
   */
  public void setWildcard(boolean wildcard) {
    this.wildcard = wildcard;
  }

  /**
   * Creates a wildcard parameter.
   *
   * @param name the parameter name
   * @return a new RouteParam
   */
  public static RouteParam wildcard(String name) {
    RouteParam param = new RouteParam();
    param.setName(name);
    param.setWildcard(true);
    return param;
  }
}
