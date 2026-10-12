package com.webforj.plugin.maven.bbj.model;

import java.io.File;
import java.util.Objects;

/**
 * Configures BBj deployment.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class BBjOptions {

  private File home;
  private File properties;
  private BBjAdminOptions admin = new BBjAdminOptions();
  private String publishName = "";
  private BBjDeploymentOptions deployment = new BBjDeploymentOptions();
  private BBjDevelopmentOptions development = new BBjDevelopmentOptions();

  /**
   * Gets the BBj installation directory.
   *
   * @return the BBj installation directory, or null for automatic detection
   */
  public File getHome() {
    return home;
  }

  /**
   * Sets the BBj installation directory.
   *
   * @param home the BBj installation directory
   *
   * @throws NullPointerException if home is null
   */
  public void setHome(File home) {
    this.home = Objects.requireNonNull(home);
  }

  /**
   * Gets the BBjServices properties file.
   *
   * @return the BBjServices properties file, or null for the file in the cfg folder of the BBj home
   */
  public File getProperties() {
    return properties;
  }

  /**
   * Sets the BBjServices properties file.
   *
   * @param properties the BBjServices properties file
   *
   * @throws NullPointerException if properties is null
   */
  public void setProperties(File properties) {
    this.properties = Objects.requireNonNull(properties);
  }

  /**
   * Gets the administration configuration.
   *
   * @return the administration configuration
   */
  public BBjAdminOptions getAdmin() {
    return admin;
  }

  /**
   * Sets the administration configuration.
   *
   * @param admin the administration configuration
   *
   * @throws NullPointerException if admin is null
   */
  public void setAdmin(BBjAdminOptions admin) {
    this.admin = Objects.requireNonNull(admin);
  }

  /**
   * Gets the published application name.
   *
   * @return the published application name
   */
  public String getPublishName() {
    return publishName;
  }

  /**
   * Sets the published application name.
   *
   * @param publishName the published application name
   *
   * @throws NullPointerException if publishName is null
   */
  public void setPublishName(String publishName) {
    this.publishName = Objects.requireNonNull(publishName);
  }

  /**
   * Gets the deployment transport settings.
   *
   * @return the transport settings
   */
  public BBjDeploymentOptions getDeployment() {
    return deployment;
  }

  /**
   * Sets the deployment transport settings.
   *
   * @param deployment the transport settings
   *
   * @throws NullPointerException if deployment is null
   */
  public void setDeployment(BBjDeploymentOptions deployment) {
    this.deployment = Objects.requireNonNull(deployment);
  }

  /**
   * Gets the development run settings.
   *
   * @return the development run settings
   */
  public BBjDevelopmentOptions getDevelopment() {
    return development;
  }

  /**
   * Sets the development run settings.
   *
   * @param development the development run settings
   *
   * @throws NullPointerException if development is null
   */
  public void setDevelopment(BBjDevelopmentOptions development) {
    this.development = Objects.requireNonNull(development);
  }
}
