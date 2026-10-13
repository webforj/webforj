package com.webforj.bbjship;

import com.basis.api.admin.BBjAdminAppDeploymentApplication;
import com.basis.api.admin.BBjAdminAppDeploymentConfiguration;
import com.basis.api.admin.BBjAdminException;
import com.webforj.bbjship.cli.admin.BBjShipClasspaths;
import com.webforj.bbjship.foundation.DeploymentArchive;
import com.webforj.bbjship.model.DeploymentRegistration;
import com.webforj.bbjship.model.InstalledDeployment;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

/**
 * Maps a deployment and its {@link ApplicationConfiguration} to the application registration and
 * the classpath of BBjServices.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
final class RegistrationMapping {

  private RegistrationMapping() {}

  /**
   * Creates the application registration of a deployment without committing it.
   *
   * @param configuration the application configuration of BBjServices
   * @param deployment the deployment
   * @param name the publish name
   * @param classpathName the classpath name of the application
   *
   * @return the registration
   * @throws BBjAdminException if BBjServices refuses a setting
   */
  static BBjAdminAppDeploymentApplication createApplication(
      BBjAdminAppDeploymentConfiguration configuration, InstalledDeployment deployment, String name,
      String classpathName) throws BBjAdminException {
    BBjAdminAppDeploymentApplication application = configuration.createApplication();
    Path program = deployment.getDirectory().resolve(DeploymentArchive.LAUNCHER_ENTRY);
    application.setString(BBjAdminAppDeploymentApplication.NAME, name);
    application.setString(BBjAdminAppDeploymentApplication.PROGRAM, program.toString());
    application.setString(BBjAdminAppDeploymentApplication.CLASSPATH, classpathName);
    application.setString(BBjAdminAppDeploymentApplication.WORKING_DIRECTORY,
        program.getParent().toString());
    application.setBoolean(BBjAdminAppDeploymentApplication.EXE_ENABLED, false);
    application.setBoolean(BBjAdminAppDeploymentApplication.BUI_ENABLED, false);
    application.setBoolean(BBjAdminAppDeploymentApplication.DWC_ENABLED, true);
    applyConfiguration(application, deployment);

    return application;
  }

  static List<String> createClasspath(InstalledDeployment deployment) {
    List<String> entries = new ArrayList<>();
    entries.add(BBjShipClasspaths.BASE_CLASSPATH_ENTRY);
    for (Path path : deployment.getClasspath()) {
      entries.add(path.toString());
    }

    return entries;
  }

  static DeploymentRegistration createRegistration(BBjAdminAppDeploymentApplication application)
      throws BBjAdminException {
    return new DeploymentRegistration(application.getProperties(), application.getArguments(),
        application.getJavaArguments());
  }

  private static void applyConfiguration(BBjAdminAppDeploymentApplication application,
      InstalledDeployment deployment) throws BBjAdminException {
    ApplicationConfiguration configuration = deployment.getConfiguration();
    Optional<Boolean> quiet = configuration.getQuietMode();
    if (quiet.isPresent()) {
      application.setBoolean(BBjAdminAppDeploymentApplication.QUIET, quiet.get());
    }

    Optional<Boolean> warning = configuration.getShowBrowserWarning();
    if (warning.isPresent()) {
      application.setBoolean(BBjAdminAppDeploymentApplication.SHOW_BROWSER_WARNING, warning.get());
      application.setBoolean(BBjAdminAppDeploymentApplication.SHOW_BROWSER_WARNING_USE_DEFAULT,
          false);
    }

    Optional<Boolean> history = configuration.getManageBrowserHistory();
    if (history.isPresent()) {
      application.setBoolean(BBjAdminAppDeploymentApplication.MANAGE_BROWSER_HISTORY,
          history.get());
      application.setBoolean(BBjAdminAppDeploymentApplication.MANAGE_BROWSER_HISTORY_USE_DEFAULT,
          false);
    }

    application.setBoolean(BBjAdminAppDeploymentApplication.SHOW_CONFIRM_CLOSE_DIALOG,
        configuration.isClientConfirmationDialogShown());
    application.setBoolean(BBjAdminAppDeploymentApplication.SHOW_CONFIRM_CLOSE_DIALOG_USE_DEFAULT,
        false);
    application.setBoolean(BBjAdminAppDeploymentApplication.DISALLOW_CONSOLE,
        configuration.isConsoleDisallowed());
    application.setBoolean(BBjAdminAppDeploymentApplication.DISALLOW_CONSOLE_USE_DEFAULT, false);
    Optional<Boolean> secure = configuration.getSecure();
    if (secure.isPresent()) {
      application.setBoolean(BBjAdminAppDeploymentApplication.SECURE, secure.get());
      application.setBoolean(BBjAdminAppDeploymentApplication.SECURE_USE_DEFAULT, false);
    }

    Optional<String> configFile = configuration.getConfigFile();
    if (configFile.isPresent()) {
      application.setString(BBjAdminAppDeploymentApplication.CONFIG_FILE, configFile.get());
    }

    List<String> arguments = configuration.getProgramArguments();
    if (!arguments.isEmpty()) {
      application.getArguments().clear();
      application.getArguments().addAll(arguments);
    }
  }
}
