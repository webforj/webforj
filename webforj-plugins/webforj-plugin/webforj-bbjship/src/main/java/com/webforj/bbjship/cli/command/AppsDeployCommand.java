package com.webforj.bbjship.cli.command;

import com.basis.api.admin.BBjAdminBase;
import com.webforj.bbjship.DeploymentInstaller;
import com.webforj.bbjship.cli.BBjShipFailureHandler;
import com.webforj.bbjship.cli.admin.BBjShipSession;
import com.webforj.bbjship.cli.option.CredentialOptions;
import com.webforj.plugin.foundation.bbj.DeploymentArchive;
import com.webforj.plugin.foundation.bbj.PublishNamePolicy;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Optional;
import java.util.OptionalInt;
import java.util.concurrent.Callable;
import picocli.CommandLine.Command;
import picocli.CommandLine.Help.Ansi.Style;
import picocli.CommandLine.Mixin;
import picocli.CommandLine.Model.CommandSpec;
import picocli.CommandLine.ParameterException;
import picocli.CommandLine.Parameters;
import picocli.CommandLine.Spec;

/**
 * Deploys an application archive under the publish name of its file name.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
@Command(name = "deploy", mixinStandardHelpOptions = true,
    description = "Deploy an application archive.")
public final class AppsDeployCommand implements Callable<Integer> {

  private static final String LABEL = "ARCHIVE";
  private static final String EXTENSION = ".zip";

  private final BBjShipSession session;

  @Spec
  private CommandSpec spec;

  @Mixin
  private CredentialOptions credentials;

  private Path archive;
  private String publishName;

  /**
   * Creates the command.
   *
   * @param session the session of the BBjShip command line
   */
  public AppsDeployCommand(BBjShipSession session) {
    this.session = session;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Integer call() throws Exception {
    String root = session.getPlugin().getDeployments().toString();
    BBjAdminBase admin = session.openAdmin(credentials, OptionalInt.empty(), OptionalInt.empty());
    try (InputStream input = Files.newInputStream(archive)) {
      DeploymentInstaller.deploy(input, publishName, root, admin);
    } finally {
      admin.release();
    }

    spec.commandLine().getOut()
        .println(spec.commandLine().getColorScheme().apply("Deployed", List.of(Style.fg_green))
            .concat(" " + publishName + " " + DeploymentInstaller.getApplicationUrl(publishName)));

    return 0;
  }

  @Parameters(paramLabel = LABEL,
      description = "Path of the deployment archive. The file name without .zip is the publish "
          + "name.")
  void setArchive(String value) {
    BBjShipFailureHandler.applyValueValidation(spec, LABEL, value);
    if (!value.endsWith(EXTENSION)) {
      throw BBjShipFailureHandler.createMissingValueException(spec, LABEL);
    }

    Path path = Path.of(value);
    String fileName = path.getFileName().toString();
    String name = fileName.substring(0, fileName.length() - EXTENSION.length());
    Optional<String> refusal = PublishNamePolicy.findRefusal(name);
    if (refusal.isPresent()) {
      throw new ParameterException(spec.commandLine(), refusal.get());
    }

    if (!Files.isRegularFile(path)) {
      throw new ParameterException(spec.commandLine(),
          "Deployment archive " + value + " does not exist");
    }

    if (!Files.isReadable(path)) {
      throw new ParameterException(spec.commandLine(),
          "Deployment archive " + value + " cannot be read");
    }

    if (isDevelopment(path, value)) {
      throw new ParameterException(spec.commandLine(),
          "Development archives are deployed by the watch, not by apps deploy");
    }

    archive = path;
    publishName = name;
  }

  private boolean isDevelopment(Path path, String value) {
    try {
      return DeploymentArchive.isDevelopment(path);
    } catch (IOException unreadable) {
      throw new ParameterException(spec.commandLine(),
          "Deployment archive " + value + " cannot be read", unreadable);
    }
  }
}
