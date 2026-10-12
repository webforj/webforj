package com.webforj.bbjship.cli.admin;

import com.basis.api.admin.BBjAdminBase;
import com.basis.api.admin.BBjAdminException;
import com.webforj.bbjship.cli.option.CredentialOptions;
import com.webforj.plugin.foundation.bbj.model.BBjCredentials;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.util.Map;
import java.util.OptionalInt;
import picocli.CommandLine.ParameterException;

/**
 * Holds what a BBjShip command reads from its caller and opens the administration session with the
 * BBjShip runtime folders, the BBjShip classpaths and the assets context in place.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class BBjShipSession {

  private final BBjShipPlugin plugin;
  private final InputStream input;
  private final Map<String, String> environment;
  private final AdminConnector connector;
  private final BBjShipServices services;
  private boolean assetsStarted;
  private BBjCredentials credentials;

  /**
   * Creates the session.
   *
   * @param plugin the BBjShip plugin of the BBj installation
   * @param input the standard input of the command
   * @param environment the environment variables of the command
   * @param connector the connector that opens the administration session
   * @param services the BBjServices of the BBj installation
   */
  public BBjShipSession(BBjShipPlugin plugin, InputStream input, Map<String, String> environment,
      AdminConnector connector, BBjShipServices services) {
    this.plugin = plugin;
    this.input = input;
    this.environment = Map.copyOf(environment);
    this.connector = connector;
    this.services = services;
  }

  /**
   * Returns the BBjShip plugin of the BBj installation.
   *
   * @return the BBjShip plugin
   */
  public BBjShipPlugin getPlugin() {
    return plugin;
  }

  /**
   * Returns the BBjServices of the BBj installation.
   *
   * @return the BBjServices of the BBj installation
   */
  public BBjShipServices getServices() {
    return services;
  }

  /**
   * Returns the standard input of the command.
   *
   * @return the standard input of the command
   */
  public InputStream getInput() {
    return input;
  }

  /**
   * Indicates whether the last opened administration session created and started the assets
   * context.
   *
   * @return true when the assets context was created and started
   */
  public boolean isAssetsStarted() {
    return assetsStarted;
  }

  /**
   * Returns the credentials of the last opened administration session.
   *
   * @return the credentials, or null when no session was opened
   */
  public BBjCredentials getCredentials() {
    return credentials;
  }

  /**
   * Opens the administration session with the resolved credentials and adds the BBjShip runtime
   * folders, the BBjShip classpaths and the assets context when they are missing.
   *
   * @param credentials the credentials options of the command
   * @param httpPort the HTTP port of the command, empty when the command has none
   * @param sslPort the SSL port of the command, empty when the command has none
   *
   * @return the authenticated session
   * @throws IOException if the plugin folder is a symbolic link, standard input cannot be read, a
   *         runtime folder cannot be created, a required file is missing, a BBjShip classpath or
   *         context belongs to another application or the BBjServices web server does not answer on
   *         a port
   * @throws BBjAdminException if BBjServices refuses the credentials, cannot be reached or refuses
   *         the configuration
   * @throws InterruptedException if the thread is interrupted while waiting for the web server
   * @throws ParameterException if a user name or password is blank or only one of them is given
   */
  public BBjAdminBase openAdmin(CredentialOptions credentials, OptionalInt httpPort,
      OptionalInt sslPort) throws IOException, BBjAdminException, InterruptedException {
    plugin.applyOwnershipValidation();
    BBjCredentials resolved = credentials.resolveCredentials(input, environment);
    BBjAdminBase admin = connector.open(resolved.getUsername(), resolved.getPassword());
    try {
      BBjShipContexts contexts = new BBjShipContexts(admin, plugin);
      new BBjShipClasspaths(admin, plugin).addMissing();
      assetsStarted = contexts.addAssets(httpPort, sslPort);
      Files.createDirectories(plugin.getDeployments());
      contexts.createDocumentRoots();
    } catch (IOException | BBjAdminException | InterruptedException | RuntimeException failure) {
      admin.release();
      throw failure;
    }

    this.credentials = resolved;

    return admin;
  }
}
