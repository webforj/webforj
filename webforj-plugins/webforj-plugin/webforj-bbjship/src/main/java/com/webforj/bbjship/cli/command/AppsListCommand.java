package com.webforj.bbjship.cli.command;

import com.basis.api.admin.BBjAdminBase;
import com.webforj.bbjship.DeploymentInstaller;
import com.webforj.bbjship.cli.admin.BBjShipPlugin;
import com.webforj.bbjship.cli.admin.BBjShipSession;
import com.webforj.bbjship.cli.option.CredentialOptions;
import com.webforj.bbjship.model.PublishedApplication;
import java.io.File;
import java.io.PrintWriter;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.OptionalInt;
import java.util.concurrent.Callable;
import java.util.stream.Collectors;
import picocli.CommandLine.Command;
import picocli.CommandLine.Help.Ansi.IStyle;
import picocli.CommandLine.Help.Ansi.Style;
import picocli.CommandLine.Help.Ansi.Text;
import picocli.CommandLine.Help.ColorScheme;
import picocli.CommandLine.Help.Column;
import picocli.CommandLine.Help.Column.Overflow;
import picocli.CommandLine.Help.TextTable;
import picocli.CommandLine.Mixin;
import picocli.CommandLine.Model.CommandSpec;
import picocli.CommandLine.Option;
import picocli.CommandLine.Spec;

/**
 * Lists the applications BBjShip deployed as a table of names and addresses.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
@Command(name = "list", mixinStandardHelpOptions = true,
    description = "List the applications BBjShip deployed.")
public final class AppsListCommand implements Callable<Integer> {

  private static final String NAME = "NAME";
  private static final String URL = "URL";
  private static final String DEPLOYMENT = "DEPLOYMENT";
  private static final String CLASS_FOLDER = "CLASS FOLDER";
  private static final int GAP = 2;

  private final BBjShipSession session;

  @Spec
  private CommandSpec spec;

  @Mixin
  private CredentialOptions credentials;

  @Option(names = "--verbose",
      description = "Also show the deployment folder of each application, relative to the plugin "
          + "folder, and the class folders of each development run.")
  private boolean verbose;

  /**
   * Creates the command.
   *
   * @param session the session of the BBjShip command line
   */
  public AppsListCommand(BBjShipSession session) {
    this.session = session;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Integer call() throws Exception {
    BBjShipPlugin plugin = session.getPlugin();
    BBjAdminBase admin = session.openAdmin(credentials, OptionalInt.empty(), OptionalInt.empty());
    List<PublishedApplication> applications;
    try {
      applications =
          DeploymentInstaller.findApplications(plugin.getDeployments().toString(), admin);
    } finally {
      admin.release();
    }

    PrintWriter out = spec.commandLine().getOut();
    if (applications.isEmpty()) {
      out.println("No applications deployed");
    } else {
      out.print(createTable(applications, plugin.getRuntime()));
    }

    return 0;
  }

  private TextTable createTable(List<PublishedApplication> applications, Path runtime) {
    int nameWidth = NAME.length();
    int urlWidth = URL.length();
    int deploymentWidth = DEPLOYMENT.length();
    int classFolderWidth = CLASS_FOLDER.length();
    for (PublishedApplication application : applications) {
      nameWidth = Math.max(nameWidth, application.getName().length());
      urlWidth = Math.max(urlWidth, application.getUrl().length());
      deploymentWidth = Math.max(deploymentWidth,
          runtime.relativize(application.getDirectory()).toString().length());
      classFolderWidth =
          Math.max(classFolderWidth, formatClassFolders(application.getClassFolders()).length());
    }

    ColorScheme colors = spec.commandLine().getColorScheme();
    List<IStyle> header = List.of(Style.bold);
    List<Column> columns = new ArrayList<>();
    List<Text> titles = new ArrayList<>();
    columns.add(new Column(nameWidth + GAP, 0, Overflow.SPAN));
    titles.add(colors.apply(NAME, header));
    columns.add(new Column(urlWidth + (verbose ? GAP : 0), 0, Overflow.SPAN));
    titles.add(colors.apply(URL, header));
    if (verbose) {
      columns.add(new Column(deploymentWidth + GAP, 0, Overflow.SPAN));
      titles.add(colors.apply(DEPLOYMENT, header));
      columns.add(new Column(classFolderWidth, 0, Overflow.SPAN));
      titles.add(colors.apply(CLASS_FOLDER, header));
    }

    TextTable table = TextTable.forColumns(colors, columns.toArray(new Column[0]));
    table.addRowValues(titles.toArray(new Text[0]));
    for (PublishedApplication application : applications) {
      List<Text> row = new ArrayList<>();
      row.add(colors.apply(application.getName(), List.of(Style.fg_green)));
      row.add(colors.apply(application.getUrl(), List.of(Style.fg_cyan)));
      if (verbose) {
        String deployment = runtime.relativize(application.getDirectory()).toString();
        row.add(colors.apply(deployment, List.of(Style.faint)));
        String classFolders = formatClassFolders(application.getClassFolders());
        row.add(colors.apply(classFolders, List.of(Style.faint)));
      }

      table.addRowValues(row.toArray(new Text[0]));
    }

    return table;
  }

  private static String formatClassFolders(List<Path> classFolders) {
    return classFolders.stream().map(Path::toString)
        .collect(Collectors.joining(File.pathSeparator));
  }
}
