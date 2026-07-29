package com.webforj.devtools.craftforj.appinfo.model;

/**
 * Environment information about the running app.
 *
 * <p>
 * Describes the app identity, framework versions and the runtime it executes on. Collected on
 * demand by the {@code appinfo.getAppInfo} action and displayed in the craftforJ frontend.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class AppInfo {

  private String appName;
  private String appClass;
  private String contextPath;
  private String projectRoot;
  private String webforjVersion;
  private boolean bbjServices;
  private String javaVersion;
  private String javaVendor;
  private String javaVm;
  private String osName;
  private String osVersion;
  private String osArch;
  private long startedAt;

  /**
   * Gets the app display name.
   *
   * @return the app name
   */
  public String getAppName() {
    return appName;
  }

  /**
   * Sets the app display name.
   *
   * @param appName the app name
   */
  public void setAppName(String appName) {
    this.appName = appName;
  }

  /**
   * Gets the fully qualified app class name.
   *
   * @return the app class name
   */
  public String getAppClass() {
    return appClass;
  }

  /**
   * Sets the fully qualified app class name.
   *
   * @param appClass the app class name
   */
  public void setAppClass(String appClass) {
    this.appClass = appClass;
  }

  /**
   * Gets the servlet context path.
   *
   * @return the context path
   */
  public String getContextPath() {
    return contextPath;
  }

  /**
   * Sets the servlet context path.
   *
   * @param contextPath the context path
   */
  public void setContextPath(String contextPath) {
    this.contextPath = contextPath;
  }

  /**
   * Gets the project root directory.
   *
   * @return the project root path
   */
  public String getProjectRoot() {
    return projectRoot;
  }

  /**
   * Sets the project root directory.
   *
   * @param projectRoot the project root path
   */
  public void setProjectRoot(String projectRoot) {
    this.projectRoot = projectRoot;
  }

  /**
   * Gets the webforJ framework version.
   *
   * @return the webforJ version, or {@code null} if unknown
   */
  public String getWebforjVersion() {
    return webforjVersion;
  }

  /**
   * Sets the webforJ framework version.
   *
   * @param webforjVersion the webforJ version
   */
  public void setWebforjVersion(String webforjVersion) {
    this.webforjVersion = webforjVersion;
  }

  /**
   * Returns whether the application runs with BBj services.
   *
   * @return {@code true} if running with BBj services
   */
  public boolean isBbjServices() {
    return bbjServices;
  }

  /**
   * Sets whether the application runs with BBj services.
   *
   * @param bbjServices {@code true} if running with BBj services
   */
  public void setBbjServices(boolean bbjServices) {
    this.bbjServices = bbjServices;
  }

  /**
   * Gets the Java runtime version.
   *
   * @return the Java version
   */
  public String getJavaVersion() {
    return javaVersion;
  }

  /**
   * Sets the Java runtime version.
   *
   * @param javaVersion the Java version
   */
  public void setJavaVersion(String javaVersion) {
    this.javaVersion = javaVersion;
  }

  /**
   * Gets the Java vendor name.
   *
   * @return the Java vendor
   */
  public String getJavaVendor() {
    return javaVendor;
  }

  /**
   * Sets the Java vendor name.
   *
   * @param javaVendor the Java vendor
   */
  public void setJavaVendor(String javaVendor) {
    this.javaVendor = javaVendor;
  }

  /**
   * Gets the Java virtual machine name.
   *
   * @return the JVM name
   */
  public String getJavaVm() {
    return javaVm;
  }

  /**
   * Sets the Java virtual machine name.
   *
   * @param javaVm the JVM name
   */
  public void setJavaVm(String javaVm) {
    this.javaVm = javaVm;
  }

  /**
   * Gets the operating system name.
   *
   * @return the OS name
   */
  public String getOsName() {
    return osName;
  }

  /**
   * Sets the operating system name.
   *
   * @param osName the OS name
   */
  public void setOsName(String osName) {
    this.osName = osName;
  }

  /**
   * Gets the operating system version.
   *
   * @return the OS version
   */
  public String getOsVersion() {
    return osVersion;
  }

  /**
   * Sets the operating system version.
   *
   * @param osVersion the OS version
   */
  public void setOsVersion(String osVersion) {
    this.osVersion = osVersion;
  }

  /**
   * Gets the operating system architecture.
   *
   * @return the OS architecture
   */
  public String getOsArch() {
    return osArch;
  }

  /**
   * Sets the operating system architecture.
   *
   * @param osArch the OS architecture
   */
  public void setOsArch(String osArch) {
    this.osArch = osArch;
  }

  /**
   * Gets the JVM start time in epoch milliseconds.
   *
   * @return the start time
   */
  public long getStartedAt() {
    return startedAt;
  }

  /**
   * Sets the JVM start time in epoch milliseconds.
   *
   * @param startedAt the start time
   */
  public void setStartedAt(long startedAt) {
    this.startedAt = startedAt;
  }
}
