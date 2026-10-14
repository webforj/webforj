package com.webforj.bbjship.cli.admin;

import com.basis.api.admin.BBjAdminBase;
import com.basis.api.admin.BBjAdminException;
import com.webforj.bbjship.AdminSessionFactory;
import com.webforj.bbjship.cli.option.CredentialOptions;
import com.webforj.bbjship.foundation.BBjShipPlugin;
import com.webforj.bbjship.foundation.model.BBjCredentials;
import java.io.IOException;
import java.nio.file.Files;
import java.time.Duration;
import java.util.Objects;
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
  private final BBjShipInput input;
  private final AdminSessionFactory sessions;
  private Duration answerDeadline = BBjShipContexts.ANSWER_DEADLINE;
  private boolean assetsStarted;

  /**
   * Creates the session.
   *
   * @param plugin the BBjShip plugin of the BBj installation
   * @param input the input of the terminal that runs the command
   * @param sessions the factory that opens the administration session
   */
  public BBjShipSession(BBjShipPlugin plugin, BBjShipInput input, AdminSessionFactory sessions) {
    this.plugin = plugin;
    this.input = input;
    this.sessions = sessions;
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
   * Returns the input of the terminal that runs the command.
   *
   * @return the input
   */
  public BBjShipInput getInput() {
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
   * Opens the administration session with the resolved credentials and adds the BBjShip runtime
   * folders, the BBjShip classpaths and the assets context when they are missing.
   *
   * @param credentials the credentials options of the command
   * @param httpPort the HTTP port of the command, empty when the command has none
   * @param sslPort the SSL port of the command, empty when the command has none
   *
   * @return the authenticated session
   * @throws IOException if the plugin folder is a symbolic link, a runtime folder cannot be
   *         created, a required file is missing, a BBjShip classpath or context belongs to another
   *         application or the BBjServices web server does not answer on a port
   * @throws BBjAdminException if BBjServices refuses the credentials, cannot be reached or refuses
   *         the configuration
   * @throws ParameterException if a user name or password is blank or only one of them is given
   */
  public BBjAdminBase openAdmin(CredentialOptions credentials, OptionalInt httpPort,
      OptionalInt sslPort) throws IOException, BBjAdminException {
    plugin.applyOwnershipValidation();
    BBjCredentials resolved = credentials.resolveCredentials(input);
    BBjAdminBase admin = sessions.create(resolved);
    try {
      BBjShipContexts contexts = createContexts(admin);
      new BBjShipClasspaths(admin, plugin).addMissing();
      assetsStarted = contexts.addAssets(httpPort, sslPort);
      Files.createDirectories(plugin.getDeployments());
      contexts.createDocumentRoots();
    } catch (IOException | BBjAdminException | RuntimeException failure) {
      admin.release();
      throw failure;
    }

    return admin;
  }
}
