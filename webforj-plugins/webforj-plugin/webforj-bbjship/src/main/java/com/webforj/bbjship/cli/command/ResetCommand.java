package com.webforj.bbjship.cli.command;

import com.basis.api.admin.BBjAdminBase;
import com.basis.api.admin.BBjAdminException;
import com.webforj.bbjship.BBjServicesClasspathRelease;
import com.webforj.bbjship.Deployer;
import com.webforj.bbjship.DeploymentFolder;
import com.webforj.bbjship.cli.admin.BBjShipClasspaths;
import com.webforj.bbjship.cli.admin.BBjShipContexts;
import com.webforj.bbjship.cli.admin.BBjShipSession;
import com.webforj.bbjship.cli.option.CredentialOptions;
import com.webforj.bbjship.model.PublishedApplication;
import com.webforj.plugin.foundation.bbj.BBjShipCommandLine;
import com.webforj.plugin.foundation.bbj.BBjShipPlugin;
import com.webforj.plugin.foundation.bbj.FileTree;
import com.webforj.plugin.foundation.bbj.ServicesProperties;
import java.io.IOException;
import java.io.PrintWriter;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.OptionalInt;
import java.util.Set;
import java.util.concurrent.Callable;
import java.util.stream.Stream;
import picocli.CommandLine;
import picocli.CommandLine.Command;
import picocli.CommandLine.Help.Ansi.Style;
import picocli.CommandLine.Mixin;
import picocli.CommandLine.Model.CommandSpec;
import picocli.CommandLine.Option;
import picocli.CommandLine.Spec;

/**
 * Takes the BBj machine back to the state right after the plugin files were placed, keeping the
 * plugin folder.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
@Command(name = BBjShipCommandLine.RESET, mixinStandardHelpOptions = true,
    description = "Undo every change BBjShip made to this BBj machine.")
public final class ResetCommand implements Callable<Integer> {

  private final BBjShipSession session;

  @Spec
  private CommandSpec spec;

  @Mixin
  private CredentialOptions credentials;

  @Option(names = BBjShipCommandLine.YES_OPTION,
      description = "Run without asking for confirmation.")
  private boolean yes;

  @Option(names = BBjShipCommandLine.PROPERTIES_OPTION, paramLabel = "FILE",
      description = "The BBjServices properties file, by default cfg/BBj.properties of the BBj "
          + "home.")
  private Path propertiesFile;

  /**
   * Creates the command.
   *
   * @param session the session of the BBjShip command line
   */
  public ResetCommand(BBjShipSession session) {
    this.session = session;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Integer call() throws Exception {
    BBjShipPlugin plugin = session.getPlugin();
    String root = plugin.getDeployments().toString();
    Path developmentSwitch = plugin.getDevelopmentSwitch();
    Path properties = propertiesFile != null ? propertiesFile
        : ServicesProperties.resolveDefaultFile(plugin.getHome());
    PrintWriter out = spec.commandLine().getOut();
    BBjAdminBase admin = session.openAdmin(credentials, OptionalInt.empty(), OptionalInt.empty());
    BBjShipContexts contexts = session.createContexts(admin);
    try {
      List<PublishedApplication> applications = Deployer.getApplications(root, admin);
      List<String> plan = new ArrayList<>();
      applications.forEach(application -> plan.add("undeploy " + application.getName()));
      boolean development = Files.exists(developmentSwitch);
      if (development) {
        plan.add("disable development runs");
      }

      boolean bbjShip = !contexts.getAddresses().isEmpty();
      boolean assets = !session.isAssetsStarted();
      if (bbjShip || assets) {
        plan.add("remove the BBjShip HTTP endpoint and assets");
      }

      boolean restore =
          Files.exists(ServicesProperties.resolveBackup(properties, plugin.getRuntime()))
              || (Files.isRegularFile(properties)
                  && ServicesProperties.load(properties, plugin.getRuntime()).hasHotswapArgument());
      if (restore) {
        plan.add("restore the Java settings of BBjServices");
      }

      if (hasUnplannedRuntimeEntries(plugin, properties)) {
        plan.add("remove the BBjShip runtime folder");
      }

      if (plan.isEmpty()) {
        removeBBjShipConfiguration(admin, contexts, false);
        FileTree.removeDirectory(plugin.getRuntime());
        out.println("Nothing to reset");
        return 0;
      }

      out.println("This resets BBjShip on this machine:");
      plan.forEach(item -> out.println("  " + item));
      if (!yes && !isConfirmed(out)) {
        out.println("Reset cancelled");
        return 1;
      }

      for (PublishedApplication application : applications) {
        Optional<String> kept = Deployer.undeploy(application.getName(), root, admin,
            new BBjServicesClasspathRelease());
        showStep("Undeployed", application.getName());
        kept.ifPresent(out::println);
      }

      if (development) {
        Files.deleteIfExists(developmentSwitch);
        showStep("Disabled", "development runs");
      }

      removeBBjShipConfiguration(admin, contexts, assets);
      boolean restored = ServicesProperties.resetJavaSettings(properties, plugin.getRuntime());
      if (restored) {
        showStep("Restored", "the Java settings of BBjServices");
      }

      removeRuntime(plugin, out);
      showStep("Reset", "BBjShip");
      if (restored) {
        out.println("Restart BBjServices now to apply the restored Java settings");
      }
    } finally {
      admin.release();
    }

    return 0;
  }

  private boolean isConfirmed(PrintWriter out) {
    out.println("Continue? [y/N]");

    return session.getInput().readLine().filter(answer -> "y".equals(answer) || "Y".equals(answer))
        .isPresent();
  }

  private static void removeRuntime(BBjShipPlugin plugin, PrintWriter out) {
    try {
      FileTree.removeDirectory(plugin.getRuntime());
    } catch (IOException held) {
      out.println(DeploymentFolder.formatKeptFolder(plugin.getRuntime(), held));
    }
  }

  private static boolean hasUnplannedRuntimeEntries(BBjShipPlugin plugin, Path properties)
      throws IOException {
    Path runtime = plugin.getRuntime();
    Set<Path> planned = Set.of(plugin.getDeployments(), plugin.getStaticFiles(), plugin.getAssets(),
        plugin.getDevelopmentSwitch(), ServicesProperties.resolveBackup(properties, runtime));
    try (Stream<Path> entries = Files.list(runtime)) {
      return entries.anyMatch(entry -> !planned.contains(entry));
    }
  }

  private void removeBBjShipConfiguration(BBjAdminBase admin, BBjShipContexts contexts,
      boolean assets) throws IOException, BBjAdminException {
    if (contexts.removeEndpoint()) {
      showStep("Disabled", "the BBjShip HTTP endpoint");
    }

    contexts.removeAssets();
    new BBjShipClasspaths(admin, session.getPlugin()).removeAll();
    if (assets) {
      showStep("Removed", "the BBjShip assets");
    }
  }

  private void showStep(String action, String subject) {
    CommandLine line = spec.commandLine();
    line.getOut().println(
        line.getColorScheme().apply(action, List.of(Style.fg_green)).concat(" " + subject));
  }
}
