package com.webforj.bbjship.cli.command;

import com.basis.api.admin.BBjAdminBase;
import com.basis.api.admin.BBjAdminException;
import com.webforj.bbjship.DeploymentInstaller;
import com.webforj.bbjship.cli.admin.BBjShipClasspaths;
import com.webforj.bbjship.cli.admin.BBjShipContexts;
import com.webforj.bbjship.cli.admin.BBjShipInput;
import com.webforj.bbjship.cli.admin.BBjShipPlugin;
import com.webforj.bbjship.cli.admin.BBjShipSession;
import com.webforj.bbjship.cli.option.CredentialOptions;
import com.webforj.bbjship.model.PublishedApplication;
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
import java.util.OptionalLong;
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
@Command(name = "reset", mixinStandardHelpOptions = true,
    description = "Undo every change BBjShip made to this BBj machine.")
public final class ResetCommand implements Callable<Integer> {

  private final BBjShipSession session;

  @Spec
  private CommandSpec spec;

  @Mixin
  private CredentialOptions credentials;

  @Option(names = "--yes", description = "Run without asking for confirmation.")
  private boolean yes;

  @Option(names = "--properties", paramLabel = "FILE",
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
        : plugin.getHome().resolve("cfg").resolve("BBj.properties");
    PrintWriter out = spec.commandLine().getOut();
    BBjAdminBase admin = session.openAdmin(credentials, OptionalInt.empty(), OptionalInt.empty());
    BBjShipContexts contexts = session.createContexts(admin);
    OptionalLong processId = OptionalLong.empty();
    boolean restored = false;
    try {
      List<PublishedApplication> applications = DeploymentInstaller.findApplications(root, admin);
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

      boolean restore = Files.exists(
          plugin.getRuntime().resolve(properties.getFileName() + ServicesProperties.BACKUP_SUFFIX));
      if (restore) {
        plan.add("restore the Java settings of BBjServices and restart it");
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
        Optional<String> kept = DeploymentInstaller.undeploy(application.getName(), root, admin);
        showStep("Undeployed", application.getName());
        kept.ifPresent(out::println);
      }

      if (development) {
        Files.deleteIfExists(developmentSwitch);
        showStep("Disabled", "development runs");
      }

      if (restore) {
        processId = session.getServices().readProcessId(admin, session.getCredentials());
      }

      removeBBjShipConfiguration(admin, contexts, assets);
      restored = ServicesProperties.resetJavaSettings(properties, plugin.getRuntime());
    } finally {
      if (!restored) {
        admin.release();
      }
    }

    if (restored) {
      showStep("Restored", "the Java settings of BBjServices");
      session.getServices().setLog(out::println);
      session.getServices().restart(admin, processId);
      showStep("Restarted", "BBjServices");
    }

    try {
      FileTree.removeDirectory(plugin.getRuntime());
    } catch (IOException held) {
      // Windows refuses to delete a jar the BBjServices virtual machine has open. Everything is
      // unregistered by now, the folder is reported and the reset ends as done
      out.println(DeploymentInstaller.formatKeptFolder(plugin.getRuntime(), held));
    }

    showStep("Reset", "BBjShip");

    return 0;
  }

  private boolean isConfirmed(PrintWriter out) throws IOException {
    out.println("Continue? [y/N]");
    String answer = BBjShipInput.readLine(session.getInput());

    return "y".equals(answer) || "Y".equals(answer);
  }

  private static boolean hasUnplannedRuntimeEntries(BBjShipPlugin plugin, Path properties)
      throws IOException {
    Path runtime = plugin.getRuntime();
    Set<Path> planned = Set.of(plugin.getDeployments(), runtime.resolve("htdocs"),
        runtime.resolve("assets"), plugin.getDevelopmentSwitch(),
        runtime.resolve(properties.getFileName() + ServicesProperties.BACKUP_SUFFIX));
    try (Stream<Path> entries = Files.list(runtime)) {
      return entries.anyMatch(entry -> !planned.contains(entry));
    }
  }

  private void removeBBjShipConfiguration(BBjAdminBase admin, BBjShipContexts contexts,
      boolean assets) throws IOException, BBjAdminException {
    if (contexts.disable()) {
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
