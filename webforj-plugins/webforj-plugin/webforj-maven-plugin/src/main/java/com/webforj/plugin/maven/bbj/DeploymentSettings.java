package com.webforj.plugin.maven.bbj;

import com.webforj.plugin.foundation.bbj.DeploymentPolicy;
import com.webforj.plugin.foundation.bbj.DeploymentPolicy.Authentication;
import com.webforj.plugin.foundation.bbj.model.BBjCredentials;
import com.webforj.plugin.maven.bbj.model.BBjOptions;
import java.io.File;
import java.net.URI;
import java.util.Objects;
import org.apache.maven.plugin.MojoExecutionException;

/**
 * Resolves the effective BBj settings of one goal from the plugin configuration and the command
 * line overrides.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class DeploymentSettings {

  private final BBjOptions options;
  private String url;
  private String publishName;
  private String defaultPublishName = "";
  private String username;
  private String password;
  private String token;
  private File home;
  private File properties;
  private String version;

  private DeploymentSettings(BBjOptions options) {
    this.options = Objects.requireNonNull(options);
  }

  /**
   * Creates settings over the configured options.
   *
   * @param options the plugin configuration
   *
   * @return the settings
   * @throws NullPointerException if options is null
   */
  public static DeploymentSettings create(BBjOptions options) {
    return new DeploymentSettings(options);
  }

  /**
   * Sets the BBjShip base address override.
   *
   * @param url the base address, null keeps the configured value
   *
   * @return these settings
   */
  public DeploymentSettings setUrl(String url) {
    this.url = url;
    return this;
  }

  /**
   * Sets the publish name override.
   *
   * @param publishName the publish name, null keeps the configured value
   *
   * @return these settings
   */
  public DeploymentSettings setPublishName(String publishName) {
    this.publishName = publishName;
    return this;
  }

  /**
   * Sets the publish name used when none is configured.
   *
   * @param defaultPublishName the publish name, null for none
   *
   * @return these settings
   */
  public DeploymentSettings setDefaultPublishName(String defaultPublishName) {
    this.defaultPublishName = defaultPublishName == null ? "" : defaultPublishName;
    return this;
  }

  /**
   * Sets the administration username override.
   *
   * @param username the username, null keeps the configured value
   *
   * @return these settings
   */
  public DeploymentSettings setUsername(String username) {
    this.username = username;
    return this;
  }

  /**
   * Sets the administration password override.
   *
   * @param password the password, null keeps the configured value
   *
   * @return these settings
   */
  public DeploymentSettings setPassword(String password) {
    this.password = password;
    return this;
  }

  /**
   * Sets the authorization token override.
   *
   * @param token the authorization token, null keeps the configured value
   *
   * @return these settings
   */
  public DeploymentSettings setToken(String token) {
    this.token = token;
    return this;
  }

  /**
   * Sets the BBj home override.
   *
   * @param home the BBj home, null keeps the configured value
   *
   * @return these settings
   */
  public DeploymentSettings setHome(File home) {
    this.home = home;
    return this;
  }

  /**
   * Sets the BBjServices properties file override.
   *
   * @param properties the properties file, null keeps the configured value
   *
   * @return these settings
   */
  public DeploymentSettings setProperties(File properties) {
    this.properties = properties;
    return this;
  }

  /**
   * Sets the webforJ version BBjShip must match.
   *
   * @param version the webforJ version
   *
   * @return these settings
   */
  public DeploymentSettings setVersion(String version) {
    this.version = version;
    return this;
  }

  /**
   * Gets the effective BBjShip base address.
   *
   * @return the base address, empty when none is set
   */
  public String getUrl() {
    return resolve(url, options.getDeployment().getUrl());
  }

  /**
   * Gets the effective publish name.
   *
   * @return the publish name, the default when none is set
   */
  public String getPublishName() {
    String name = resolve(publishName, options.getPublishName());
    return name.isBlank() ? defaultPublishName : name;
  }

  /**
   * Gets the effective administration credentials.
   *
   * <p>
   * A token that is not blank takes precedence over the username and password. Without a username
   * and without a password, the credentials are the factory administrator account of BBj.
   * </p>
   *
   * @return the credentials
   * @throws MojoExecutionException if only one of username and password is given or one is blank
   */
  public BBjCredentials getCredentials() throws MojoExecutionException {
    return createCredentials(Authentication.PASSWORD_OR_TOKEN);
  }

  /**
   * Gets the administration username and password, ignoring a configured token.
   *
   * <p>
   * Without a username and without a password, the credentials are the factory administrator
   * account of BBj.
   * </p>
   *
   * @return the username and password credentials
   * @throws MojoExecutionException if only one of username and password is given or one is blank
   */
  public BBjCredentials getPasswordCredentials() throws MojoExecutionException {
    return createCredentials(Authentication.PASSWORD);
  }

  /**
   * Gets the webforJ version BBjShip must match.
   *
   * @return the version
   */
  public String getVersion() {
    return version;
  }

  /**
   * Gets the effective BBj home.
   *
   * @return the BBj home, null when none is set
   */
  public File getHome() {
    return resolve(home, options.getHome());
  }

  /**
   * Gets the effective BBjServices properties file.
   *
   * @return the properties file, null when none is set
   */
  public File getProperties() {
    return resolve(properties, options.getProperties());
  }

  /**
   * Gets the web server port of the effective BBjShip base address.
   *
   * @return the port of the address, or the default port of its scheme
   * @throws IllegalArgumentException if the base address is not a valid URI
   */
  public int getPort() {
    return DeploymentPolicy.getPort(URI.create(getUrl()));
  }

  private BBjCredentials createCredentials(Authentication authentication)
      throws MojoExecutionException {
    try {
      return DeploymentPolicy.resolveCredentials(
          resolve(username, options.getAdmin().getUsername()),
          resolve(password, options.getAdmin().getPassword()),
          resolve(token, options.getAdmin().getToken()), authentication);
    } catch (IllegalArgumentException refusal) {
      throw new MojoExecutionException(refusal.getMessage(), refusal);
    }
  }

  private static <T> T resolve(T override, T configured) {
    return override == null ? configured : override;
  }
}
