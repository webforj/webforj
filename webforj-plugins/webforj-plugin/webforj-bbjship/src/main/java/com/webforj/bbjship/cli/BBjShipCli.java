package com.webforj.bbjship.cli;

import com.basis.api.admin.BBjAdminFactory;
import com.webforj.bbjship.cli.admin.AdminConnector;
import com.webforj.bbjship.cli.admin.BBjShipPlugin;
import com.webforj.bbjship.cli.admin.BBjShipServices;
import com.webforj.bbjship.cli.admin.BBjShipSession;
import com.webforj.bbjship.cli.command.AppsDeployCommand;
import com.webforj.bbjship.cli.command.AppsListCommand;
import com.webforj.bbjship.cli.command.AppsUndeployCommand;
import com.webforj.bbjship.cli.command.BBjShipCommandGroup;
import com.webforj.bbjship.cli.command.DevelopmentDisableCommand;
import com.webforj.bbjship.cli.command.DevelopmentEnableCommand;
import com.webforj.bbjship.cli.command.RemoteDisableCommand;
import com.webforj.bbjship.cli.command.RemoteEnableCommand;
import com.webforj.bbjship.cli.command.RemoteStatusCommand;
import com.webforj.bbjship.cli.command.ResetCommand;
import com.webforj.bbjship.cli.command.ServicesRestartCommand;
import java.io.InputStream;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.nio.file.Path;
import java.util.Map;
import java.util.Objects;
import picocli.CommandLine;
import picocli.CommandLine.Help;
import picocli.CommandLine.Help.Ansi;
import picocli.CommandLine.Model.CommandSpec;

/**
 * Runs the command line of the BBjShip plugin.
 *
 * <p>
 * The {@code bbjship} launcher of the plugin starts this class with the Java runtime BBj uses for
 * its own tools and the BBj home in the {@code basis.BBjHome} system property. The commands reach
 * BBjServices through the administration API. Colors follow the terminal, they are off when the
 * output is redirected or {@code NO_COLOR} is set.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class BBjShipCli {

  private static final String HOME_PROPERTY = "basis.BBjHome";

  private final BBjShipSession session;
  private final PrintStream output;
  private final PrintStream error;
  private final Ansi ansi;

  private BBjShipCli(Builder builder) {
    this.session = new BBjShipSession(new BBjShipPlugin(builder.home), builder.input,
        builder.environment, builder.connector, builder.services);
    this.output = builder.output;
    this.error = builder.error;
    this.ansi = builder.ansi;
  }

  /**
   * Runs the BBjShip command line and exits with its status.
   *
   * @param arguments the command line arguments
   */
  @SuppressWarnings("java:S106")
  public static void main(String[] arguments) {
    System.exit(create(Path.of(System.getProperty(HOME_PROPERTY))).setInput(System.in)
        .setEnvironment(System.getenv()).setOutput(System.out).setError(System.err).build()
        .run(arguments));
  }

  /**
   * Creates a builder for the command line of the BBjShip plugin.
   *
   * @param home the BBj installation folder
   *
   * @return the builder
   */
  public static Builder create(Path home) {
    return new Builder(home);
  }

  /**
   * Runs one command.
   *
   * @param arguments the command line arguments
   *
   * @return 0 on success, 2 for invalid arguments, 1 when the operation failed
   */
  public int run(String... arguments) {
    PrintWriter out = new PrintWriter(output, true);
    PrintWriter err = new PrintWriter(error, true);
    try {
      return createCommandLine(out, err).execute(arguments);
    } finally {
      out.flush();
      err.flush();
    }
  }

  private CommandLine createCommandLine(PrintWriter out, PrintWriter err) {
    CommandLine root = new CommandLine(new BBjShipCommandGroup());
    root.getCommandSpec().name("bbjship").usageMessage()
        .description("Configure webforJ BBjShip and manage the applications it deployed.")
        .footer("",
            "WEBFORJ_BBJ_USERNAME and WEBFORJ_BBJ_PASSWORD are used when the options are "
                + "absent. Without options and variables, the BBj factory administrator account is "
                + "used.");
    CommandLine remote = new CommandLine(new BBjShipCommandGroup());
    remote.getCommandSpec().usageMessage().description("Manage the BBjShip HTTP endpoint.");
    remote.addSubcommand(new RemoteEnableCommand(session));
    remote.addSubcommand(new RemoteDisableCommand(session));
    remote.addSubcommand(new RemoteStatusCommand(session));
    root.addSubcommand("remote", remote);
    CommandLine development = new CommandLine(new BBjShipCommandGroup());
    development.getCommandSpec().usageMessage()
        .description("Manage development runs on this BBj machine.");
    development.addSubcommand(new DevelopmentEnableCommand(session));
    development.addSubcommand(new DevelopmentDisableCommand(session));
    root.addSubcommand("development", development);
    CommandLine services = new CommandLine(new BBjShipCommandGroup());
    services.getCommandSpec().usageMessage().description("Manage the local BBjServices.");
    services.addSubcommand(new ServicesRestartCommand(session));
    root.addSubcommand("services", services);
    CommandLine apps = new CommandLine(new BBjShipCommandGroup());
    apps.getCommandSpec().usageMessage()
        .description("Deploy, list and undeploy the applications BBjShip deployed.");
    apps.addSubcommand(new AppsDeployCommand(session));
    apps.addSubcommand(new AppsListCommand(session));
    apps.addSubcommand(new AppsUndeployCommand(session));
    root.addSubcommand("apps", apps);
    root.addSubcommand(new ResetCommand(session));
    applyVersion(root);
    BBjShipFailureHandler failures = new BBjShipFailureHandler();
    root.setOut(out).setErr(err).setColorScheme(Help.defaultColorScheme(ansi))
        .setParameterExceptionHandler(failures).setExecutionExceptionHandler(failures);

    return root;
  }

  private void applyVersion(CommandLine line) {
    CommandSpec spec = line.getCommandSpec();
    spec.versionProvider(
        () -> new String[] {"webforJ BBjShip " + session.getPlugin().readVersion()});
    line.getSubcommands().values().forEach(this::applyVersion);
  }

  /**
   * Builds a {@link BBjShipCli}.
   *
   * @author Hyyan Abo Fakher
   * @since 26.03
   */
  public static final class Builder {

    private final Path home;
    private InputStream input;
    private Map<String, String> environment;
    private PrintStream output;
    private PrintStream error;
    private Ansi ansi = Ansi.AUTO;
    private AdminConnector connector = BBjAdminFactory::getBBjAdmin;
    private BBjShipServices services;

    private Builder(Path home) {
      this.home = Objects.requireNonNull(home);
      this.services = new BBjShipServices(new BBjShipPlugin(home));
    }

    /**
     * Sets the standard input of the command.
     *
     * @param input the standard input of the command
     *
     * @return this builder
     */
    public Builder setInput(InputStream input) {
      this.input = Objects.requireNonNull(input);
      return this;
    }

    /**
     * Sets the environment variables of the command.
     *
     * @param environment the environment variables of the command
     *
     * @return this builder
     */
    public Builder setEnvironment(Map<String, String> environment) {
      this.environment = Objects.requireNonNull(environment);
      return this;
    }

    /**
     * Sets the standard output stream.
     *
     * @param output the standard output stream
     *
     * @return this builder
     */
    public Builder setOutput(PrintStream output) {
      this.output = Objects.requireNonNull(output);
      return this;
    }

    /**
     * Sets the standard error stream.
     *
     * @param error the standard error stream
     *
     * @return this builder
     */
    public Builder setError(PrintStream error) {
      this.error = Objects.requireNonNull(error);
      return this;
    }

    /**
     * Sets the color mode.
     *
     * @param ansi the color mode
     *
     * @return this builder
     */
    public Builder setAnsi(Ansi ansi) {
      this.ansi = Objects.requireNonNull(ansi);
      return this;
    }

    /**
     * Sets the connector that opens the administration session.
     *
     * @param connector the connector that opens the administration session
     *
     * @return this builder
     */
    public Builder setConnector(AdminConnector connector) {
      this.connector = Objects.requireNonNull(connector);
      return this;
    }

    /**
     * Sets the BBjServices the commands restart.
     *
     * @param services the BBjServices the commands restart
     *
     * @return this builder
     */
    public Builder setServices(BBjShipServices services) {
      this.services = Objects.requireNonNull(services);
      return this;
    }

    /**
     * Builds the command line.
     *
     * @return the command line
     */
    public BBjShipCli build() {
      Objects.requireNonNull(input);
      Objects.requireNonNull(environment);
      Objects.requireNonNull(output);
      Objects.requireNonNull(error);

      return new BBjShipCli(this);
    }
  }
}
