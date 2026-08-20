package com.webforj.devtools.craftforj.router.model;

import java.util.ArrayList;
import java.util.List;

/**
 * Route information.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class RouteInfo {

  private String id;
  private String path;
  private String componentType;
  private String displayName;
  private RouteType type;
  private String outletType;
  private String frameId;
  private int priority;
  private String frameTitle;
  private List<RouteAliasInfo> aliases = new ArrayList<>();
  private List<RouteParam> params = new ArrayList<>();
  private SecurityAccess security;
  private List<String> allowedRoles = new ArrayList<>();
  private boolean hasWillEnter;
  private boolean hasDidEnter;
  private boolean hasWillLeave;
  private boolean hasDidLeave;
  private boolean hasActivate;
  private String sourceFile;
  private boolean kotlin;
  private boolean isActive;
  private List<RouteInfo> children = new ArrayList<>();

  /**
   * Gets the unique identifier for this route.
   *
   * @return the route ID
   */
  public String getId() {
    return id;
  }

  /**
   * Sets the unique identifier for this route.
   *
   * @param id the route ID
   */
  public void setId(String id) {
    this.id = id;
  }

  /**
   * Gets the route pattern.
   *
   * @return the path
   */
  public String getPath() {
    return path;
  }

  /**
   * Sets the route pattern.
   *
   * @param path the path
   */
  public void setPath(String path) {
    this.path = path;
  }

  /**
   * Gets the fully qualified component class name.
   *
   * @return the component type
   */
  public String getComponentType() {
    return componentType;
  }

  /**
   * Sets the fully qualified component class name.
   *
   * @param componentType the component type
   */
  public void setComponentType(String componentType) {
    this.componentType = componentType;
  }

  /**
   * Gets the simple class name for display.
   *
   * @return the display name
   */
  public String getDisplayName() {
    return displayName;
  }

  /**
   * Sets the simple class name for display.
   *
   * @param displayName the display name
   */
  public void setDisplayName(String displayName) {
    this.displayName = displayName;
  }

  /**
   * Gets the route type (VIEW or LAYOUT).
   *
   * @return the type
   */
  public RouteType getType() {
    return type;
  }

  /**
   * Sets the route type.
   *
   * @param type the type
   */
  public void setType(RouteType type) {
    this.type = type;
  }

  /**
   * Gets the outlet class name.
   *
   * @return the outlet type
   */
  public String getOutletType() {
    return outletType;
  }

  /**
   * Sets the outlet class name.
   *
   * @param outletType the outlet type
   */
  public void setOutletType(String outletType) {
    this.outletType = outletType;
  }

  /**
   * Gets the frame ID.
   *
   * @return the frame ID or null
   */
  public String getFrameId() {
    return frameId;
  }

  /**
   * Sets the frame ID.
   *
   * @param frameId the frame ID
   */
  public void setFrameId(String frameId) {
    this.frameId = frameId;
  }

  /**
   * Gets the route matching priority.
   *
   * @return the priority
   */
  public int getPriority() {
    return priority;
  }

  /**
   * Sets the route matching priority.
   *
   * @param priority the priority
   */
  public void setPriority(int priority) {
    this.priority = priority;
  }

  /**
   * Gets the frame title from @FrameTitle annotation.
   *
   * @return the frame title or null
   */
  public String getFrameTitle() {
    return frameTitle;
  }

  /**
   * Sets the frame title.
   *
   * @param frameTitle the frame title
   */
  public void setFrameTitle(String frameTitle) {
    this.frameTitle = frameTitle;
  }

  /**
   * Gets the list of route aliases.
   *
   * @return the aliases
   */
  public List<RouteAliasInfo> getAliases() {
    return aliases;
  }

  /**
   * Sets the list of route aliases.
   *
   * @param aliases the aliases
   */
  public void setAliases(List<RouteAliasInfo> aliases) {
    this.aliases = aliases;
  }

  /**
   * Gets the list of route parameters.
   *
   * @return the params
   */
  public List<RouteParam> getParams() {
    return params;
  }

  /**
   * Sets the list of route parameters.
   *
   * @param params the params
   */
  public void setParams(List<RouteParam> params) {
    this.params = params;
  }

  /**
   * Gets the security access type.
   *
   * @return the security
   */
  public SecurityAccess getSecurity() {
    return security;
  }

  /**
   * Sets the security access type.
   *
   * @param security the security
   */
  public void setSecurity(SecurityAccess security) {
    this.security = security;
  }

  /**
   * Gets the list of allowed roles from @RolesAllowed.
   *
   * @return the allowed roles
   */
  public List<String> getAllowedRoles() {
    return allowedRoles;
  }

  /**
   * Sets the list of allowed roles.
   *
   * @param allowedRoles the allowed roles
   */
  public void setAllowedRoles(List<String> allowedRoles) {
    this.allowedRoles = allowedRoles;
  }

  /**
   * Checks if component implements WillEnterObserver.
   *
   * @return true if implements WillEnterObserver
   */
  public boolean hasWillEnterObserver() {
    return hasWillEnter;
  }

  /**
   * Sets whether component implements WillEnterObserver.
   *
   * @param hasWillEnter true if implements WillEnterObserver
   */
  public void setHasWillEnter(boolean hasWillEnter) {
    this.hasWillEnter = hasWillEnter;
  }

  /**
   * Checks if component implements DidEnterObserver.
   *
   * @return true if implements DidEnterObserver
   */
  public boolean hasDidEnterObserver() {
    return hasDidEnter;
  }

  /**
   * Sets whether component implements DidEnterObserver.
   *
   * @param hasDidEnter true if implements DidEnterObserver
   */
  public void setHasDidEnter(boolean hasDidEnter) {
    this.hasDidEnter = hasDidEnter;
  }

  /**
   * Checks if component implements WillLeaveObserver.
   *
   * @return true if implements WillLeaveObserver
   */
  public boolean hasWillLeaveObserver() {
    return hasWillLeave;
  }

  /**
   * Sets whether component implements WillLeaveObserver.
   *
   * @param hasWillLeave true if implements WillLeaveObserver
   */
  public void setHasWillLeave(boolean hasWillLeave) {
    this.hasWillLeave = hasWillLeave;
  }

  /**
   * Checks if component implements DidLeaveObserver.
   *
   * @return true if implements DidLeaveObserver
   */
  public boolean hasDidLeaveObserver() {
    return hasDidLeave;
  }

  /**
   * Sets whether component implements DidLeaveObserver.
   *
   * @param hasDidLeave true if implements DidLeaveObserver
   */
  public void setHasDidLeave(boolean hasDidLeave) {
    this.hasDidLeave = hasDidLeave;
  }

  /**
   * Checks if component implements ActivateObserver.
   *
   * @return true if implements ActivateObserver
   */
  public boolean hasActivateObserver() {
    return hasActivate;
  }

  /**
   * Sets whether component implements ActivateObserver.
   *
   * @param hasActivate true if implements ActivateObserver
   */
  public void setHasActivate(boolean hasActivate) {
    this.hasActivate = hasActivate;
  }

  /**
   * Gets the absolute path to source file.
   *
   * @return the source file path or null
   */
  public String getSourceFile() {
    return sourceFile;
  }

  /**
   * Sets the absolute path to source file.
   *
   * @param sourceFile the source file path
   */
  public void setSourceFile(String sourceFile) {
    this.sourceFile = sourceFile;
  }

  /**
   * Checks whether the route class was compiled from Kotlin.
   *
   * @return {@code true} for a Kotlin route class.
   */
  public boolean isKotlin() {
    return kotlin;
  }

  /**
   * Sets whether the route class was compiled from Kotlin.
   *
   * @param kotlin {@code true} for a Kotlin route class
   */
  public void setKotlin(boolean kotlin) {
    this.kotlin = kotlin;
  }

  /**
   * Checks if this route is currently rendered.
   *
   * @return true if active
   */
  public boolean isActive() {
    return isActive;
  }

  /**
   * Sets whether this route is currently rendered.
   *
   * @param isActive true if active
   */
  public void setActive(boolean isActive) {
    this.isActive = isActive;
  }

  /**
   * Gets the child routes.
   *
   * @return the children
   */
  public List<RouteInfo> getChildren() {
    return children;
  }

  /**
   * Sets the child routes.
   *
   * @param children the children
   */
  public void setChildren(List<RouteInfo> children) {
    this.children = children;
  }
}
