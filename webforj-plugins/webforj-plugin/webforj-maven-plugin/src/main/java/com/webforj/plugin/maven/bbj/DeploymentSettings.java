package com.webforj.plugin.maven.bbj;

import com.webforj.bbjship.foundation.BBjShipClient;
import com.webforj.bbjship.foundation.DeploymentPolicy;
import com.webforj.bbjship.foundation.DeploymentPolicy.Authentication;
import com.webforj.bbjship.foundation.model.BBjCredentials;
import com.webforj.plugin.maven.bbj.model.BBjOptions;
import java.io.File;
import java.net.URI;
import java.util.Objects;
import java.util.function.Consumer;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;

/**
 * Resolves the effective BBj settings of one goal from the plugin configuration and the command
 * line overrides.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class DeploymentSettings {

  private static final String SUPPLY = "Configure bbj.admin.username and bbj.admin.password";
  private static final String TOKEN_SUPPLY = SUPPLY + ", or webforj.bbj.admin.token";

  private final BBjOptions options;
  private String url;
  private Boolean allowUnencrypted;
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
   * Sets the override of whether an unencrypted connection to another machine is allowed.
   *
   * @param allowUnencrypted the allowance, null keeps the configured value
   *
   * @return these settings
   */
  public DeploymentSettings setAllowUnencrypted(Boolean allowUnencrypted) {
    this.allowUnencrypted = allowUnencrypted;
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
   * Checks whether the administration credentials may travel over an unencrypted connection to
   * another machine.
   *
   * @return true when an unencrypted connection to another machine is allowed
   */
  public boolean isAllowUnencrypted() {
    return resolve(allowUnencrypted, options.getDeployment().isAllowUnencrypted());
  }

  /**
   * Resolves the effective BBjShip base address.
   *
   * @return the base address
   * @throws MojoExecutionException if no base address is set
   * @throws IllegalArgumentException if the base address is not a valid URI
   */
  public URI resolveUrl() throws MojoExecutionException {
    String effective = getUrl();
    if (effective.isBlank()) {
      throw new MojoExecutionException("BBj deployment requires bbj.deployment.url. Set it in "
          + "plugin configuration or pass -Dwebforj.bbj.deployment.url=<URL>");
    }

    return URI.create(effective);
  }

  /**
   * Creates the client of the effective BBjShip with the effective credentials and version.
   *
   * <p>
   * The warning of the factory administrator account, the progress and the warnings of the client
   * are reported to the log.
   * </p>
   *
   * @param log the log of the goal
   *
   * @return the client
   * @throws MojoExecutionException if no base address is set, or only one of username and password
   *         is given or one is blank
   * @throws IllegalArgumentException if the base address is no valid BBjShip address
   */
  public BBjShipClient createBBjShipClient(Log log) throws MojoExecutionException {
    URI effective = resolveUrl();
    BBjCredentials credentials = getCredentials(log::warn);

    return BBjShipClient.create(effective).setCredentials(credentials).setVersion(version)
        .setLog(log::info).setWarn(log::warn).setAllowUnencrypted(isAllowUnencrypted()).build();
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
   * @param warn where the warning of the factory administrator account is reported
   *
   * @return the credentials
   * @throws MojoExecutionException if only one of username and password is given or one is blank
   */
  public BBjCredentials getCredentials(Consumer<String> warn) throws MojoExecutionException {
    return createCredentials(Authentication.PASSWORD_OR_TOKEN, warn);
  }

  /**
   * Gets the administration username and password, ignoring a configured token.
   *
   * <p>
   * Without a username and without a password, the credentials are the factory administrator
   * account of BBj.
   * </p>
   *
   * @param warn where the warning of the factory administrator account is reported
   *
   * @return the username and password credentials
   * @throws MojoExecutionException if only one of username and password is given or one is blank
   */
  public BBjCredentials getPasswordCredentials(Consumer<String> warn)
      throws MojoExecutionException {
    return createCredentials(Authentication.PASSWORD, warn);
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
    return BBjShipClient.getPort(URI.create(getUrl()));
  }

  private BBjCredentials createCredentials(Authentication authentication, Consumer<String> warn)
      throws MojoExecutionException {
    try {
      return DeploymentPolicy.resolveCredentials(
          resolve(username, options.getAdmin().getUsername()),
          resolve(password, options.getAdmin().getPassword()),
          resolve(token, options.getAdmin().getToken()), authentication, warn);
    } catch (IllegalArgumentException refusal) {
      String supply = authentication == Authentication.PASSWORD_OR_TOKEN ? TOKEN_SUPPLY : SUPPLY;
      throw new MojoExecutionException(refusal.getMessage() + ". " + supply, refusal);
    }
  }

  private static <T> T resolve(T override, T configured) {
    return override == null ? configured : override;
  }
}
