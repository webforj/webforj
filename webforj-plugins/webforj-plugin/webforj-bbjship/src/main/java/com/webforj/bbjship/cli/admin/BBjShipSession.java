package com.webforj.bbjship.cli.admin;

import com.basis.api.admin.BBjAdminBase;
import com.basis.api.admin.BBjAdminException;
import com.webforj.bbjship.DeploymentInstaller;
import com.webforj.bbjship.cli.option.CredentialOptions;
import com.webforj.plugin.foundation.bbj.model.BBjCredentials;
import java.io.IOException;
import java.io.InputStream;
import java.lang.System.Logger;
import java.lang.System.Logger.Level;
import java.net.URI;
import java.net.URLEncoder;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.net.http.HttpResponse.BodyHandlers;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.time.Duration;
import java.util.Base64;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
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

  private static final Logger LOGGER = System.getLogger(BBjShipSession.class.getName());
  private static final Duration ENDPOINT_TIMEOUT = Duration.ofSeconds(30);
  private static final int HTTP_NO_CONTENT = 204;

  private final BBjShipPlugin plugin;
  private final InputStream input;
  private final Map<String, String> environment;
  private final AdminConnector connector;
  private final BBjShipServices services;
  private Duration answerDeadline = BBjShipContexts.ANSWER_DEADLINE;
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
   * Creates the BBjShip contexts of an administration session, waiting for the BBjServices web
   * server as long as this session allows.
   *
   * @param admin the administration session
   *
   * @return the contexts
   */
  public BBjShipContexts createContexts(BBjAdminBase admin) {
    return new BBjShipContexts(admin, plugin, answerDeadline);
  }

  /**
   * Finds the BBjShip endpoint of this machine an undeploy can be sent to, so that it runs inside
   * BBjServices and releases the files of the application before their removal.
   *
   * @param admin the administration session
   *
   * @return the HTTP address of the endpoint, empty when there is none, it belongs to another
   *         application or BBjServices does not answer for it
   */
  public Optional<URI> findUndeployEndpoint(BBjAdminBase admin) {
    try {
      return createContexts(admin).getAddresses().stream()
          .filter(address -> "http".equals(address.getScheme())).findFirst();
    } catch (IOException | BBjAdminException unavailable) {
      LOGGER.log(Level.DEBUG, "The BBjShip endpoint is not available for an undeployment",
          unavailable);
      return Optional.empty();
    }
  }

  /**
   * Undeploys an application. With an endpoint, the undeploy is sent to it and runs inside
   * BBjServices, which releases the files of the application before their removal. When the
   * endpoint does not answer, or without one, it runs through the administration session, and a
   * folder whose files cannot be removed stays.
   *
   * @param admin the administration session
   * @param endpoint the endpoint of {@link #findUndeployEndpoint(BBjAdminBase)}
   * @param publishName the published application name
   *
   * @return the report naming the folder that stayed and why, empty when it is removed
   * @throws IOException if the application is not deployed by BBjShip or the unregistration fails
   */
  public Optional<String> undeploy(BBjAdminBase admin, Optional<URI> endpoint, String publishName)
      throws IOException {
    String root = plugin.getDeployments().toString();
    if (endpoint.isPresent() && (isUndeployedThroughEndpoint(endpoint.get(), publishName)
        || isUndeployed(admin, root, publishName))) {
      return Optional.empty();
    }

    return DeploymentInstaller.undeploy(publishName, root, admin);
  }

  /**
   * Sets the longest wait for the BBjServices web server to answer on a port of a context.
   *
   * @param answerDeadline the longest wait
   *
   * @throws NullPointerException if answerDeadline is null
   */
  public void setAnswerDeadline(Duration answerDeadline) {
    this.answerDeadline = Objects.requireNonNull(answerDeadline);
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
      BBjShipContexts contexts = createContexts(admin);
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

  private boolean isUndeployedThroughEndpoint(URI endpoint, String publishName) throws IOException {
    HttpRequest request = HttpRequest
        .newBuilder(URI.create(endpoint + "/applications/"
            + URLEncoder.encode(publishName, StandardCharsets.UTF_8).replace("+", "%20")))
        .DELETE().header("Authorization", formatAuthorization()).timeout(ENDPOINT_TIMEOUT).build();
    try (HttpClient client = HttpClient.newBuilder().connectTimeout(ENDPOINT_TIMEOUT).build()) {
      HttpResponse<Void> answer = client.send(request, BodyHandlers.discarding());
      if (answer.statusCode() == HTTP_NO_CONTENT) {
        return true;
      }

      LOGGER.log(Level.DEBUG, "The BBjShip endpoint answered HTTP " + answer.statusCode()
          + " for the undeployment of " + publishName);
    } catch (IOException unanswered) {
      // The endpoint is registered but does not answer, the undeploy runs through the
      // administration session instead
      LOGGER.log(Level.DEBUG,
          "The BBjShip endpoint did not answer for the undeployment of " + publishName, unanswered);
    } catch (InterruptedException interruption) {
      Thread.currentThread().interrupt();
      throw new IOException("The undeployment of " + publishName + " was interrupted",
          interruption);
    }

    return false;
  }

  private String formatAuthorization() {
    return "Basic " + Base64.getEncoder()
        .encodeToString((credentials.getUsername() + ":" + credentials.getPassword())
            .getBytes(StandardCharsets.UTF_8));
  }

  private static boolean isUndeployed(BBjAdminBase admin, String root, String publishName)
      throws IOException {
    // An endpoint that did not answer in time may have finished the undeploy after all
    return DeploymentInstaller.findApplications(root, admin).stream()
        .noneMatch(application -> publishName.equals(application.getName()));
  }
}
