package com.webforj.bbjship.cli.command;

import com.basis.api.admin.BBjAdminBase;
import com.webforj.bbjship.BBjServicesClasspathRelease;
import com.webforj.bbjship.Deployer;
import com.webforj.bbjship.cli.admin.BBjShipSession;
import com.webforj.bbjship.cli.option.CredentialOptions;
import com.webforj.bbjship.foundation.BBjShipCommandLine;
import com.webforj.bbjship.foundation.BBjShipPlugin;
import com.webforj.bbjship.model.PublishedApplication;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.List;
import java.util.Optional;
import java.util.OptionalInt;
import java.util.concurrent.Callable;
import picocli.CommandLine.ArgGroup;
import picocli.CommandLine.Command;
import picocli.CommandLine.Help.Ansi.Style;
import picocli.CommandLine.Mixin;
import picocli.CommandLine.Model.CommandSpec;
import picocli.CommandLine.Option;
import picocli.CommandLine.Parameters;
import picocli.CommandLine.Spec;

/**
 * Undeploys one application BBjShip deployed, or all of them.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
@Command(name = BBjShipCommandLine.UNDEPLOY, mixinStandardHelpOptions = true,
    description = "Undeploy an application BBjShip deployed, or all with "
        + BBjShipCommandLine.ALL_OPTION + ".")
public final class AppsUndeployCommand implements Callable<Integer> {

  private final BBjShipSession session;

  @Spec
  private CommandSpec spec;

  @Mixin
  private CredentialOptions credentials;

  @ArgGroup(exclusive = true, multiplicity = "1")
  private Target target;

  /**
   * Creates the command.
   *
   * @param session the session of the BBjShip command line
   */
  public AppsUndeployCommand(BBjShipSession session) {
    this.session = session;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Integer call() throws Exception {
    BBjShipPlugin plugin = session.getPlugin();
    String root = plugin.getDeployments().toString();
    PrintWriter out = spec.commandLine().getOut();
    BBjAdminBase admin = session.openAdmin(credentials, OptionalInt.empty(), OptionalInt.empty());
    try {
      boolean found = false;
      for (PublishedApplication application : Deployer.getApplications(root, admin)) {
        if (target.all || application.getName().equals(target.name)) {
          Optional<String> kept = Deployer.undeploy(application.getName(), root, admin,
              new BBjServicesClasspathRelease());
          out.println(spec.commandLine().getColorScheme()
              .apply("Undeployed", List.of(Style.fg_green)).concat(" " + application.getName()));
          kept.ifPresent(out::println);
          found = true;
        }
      }

      if (!target.all && !found) {
        throw new IOException("Application " + target.name + " is not deployed by BBjShip");
      }
    } finally {
      admin.release();
    }

    return 0;
  }

  /**
   * Holds the application name or the request to undeploy every application.
   *
   * @author Hyyan Abo Fakher
   * @since 26.03
   */
  private static final class Target {

    @Parameters(paramLabel = "NAME", description = "Publish name of the application.")
    private String name;

    @Option(names = BBjShipCommandLine.ALL_OPTION,
        description = "Undeploy every application BBjShip deployed.")
    private boolean all;
  }
}
