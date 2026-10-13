package com.webforj.bbjship.cli;

import com.webforj.bbjship.AdminSessionFactory;
import com.webforj.bbjship.BBjServicesAdminSessionFactory;
import com.webforj.bbjship.cli.admin.BBjShipInput;
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
import com.webforj.bbjship.cli.command.ServicesStopCommand;
import com.webforj.bbjship.foundation.BBjShipCommandLine;
import com.webforj.bbjship.foundation.BBjShipPlugin;
import java.io.PrintWriter;
import java.io.Writer;
import java.nio.file.Path;
import java.time.Duration;
import java.util.Objects;
import java.util.function.Consumer;
import picocli.CommandLine;
import picocli.CommandLine.Help;
import picocli.CommandLine.Help.Ansi;
import picocli.CommandLine.Model.CommandSpec;

/**
 * Runs the command line of the BBjShip plugin inside BBjServices.
 *
 * <p>
 * The BBj program {@code bbjship.bbj} hands the arguments and the terminal to
 * {@link #run(String[], BBjShipTerminal)}. The commands reach BBjServices through the
 * administration API.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class BBjShipCli {

  private final BBjShipSession session;
  private final BBjShipTerminal terminal;
  private final Ansi ansi;

  private BBjShipCli(Builder builder) {
    BBjShipInput input = new BBjShipInput(builder.terminal);
    if (builder.operatingSystem != null) {
      input.setOperatingSystem(builder.operatingSystem);
    }

    this.session = new BBjShipSession(builder.plugin, input, builder.sessions);
    if (builder.answerDeadline != null) {
      this.session.setAnswerDeadline(builder.answerDeadline);
    }

    this.terminal = builder.terminal;
    this.ansi = builder.ansi != null ? builder.ansi : resolveAnsi(builder.terminal);
  }

  /**
   * Creates a builder for the command line of the BBjShip plugin.
   *
   * @param plugin the BBjShip plugin of the BBj installation
   *
   * @return the builder
   */
  public static Builder create(BBjShipPlugin plugin) {
    return new Builder(plugin);
  }

  /**
   * Runs one command inside BBjServices for the BBj installation that runs it.
   *
   * @param arguments the command line arguments
   * @param terminal the terminal that runs the command
   *
   * @return 0 on success, 2 for invalid arguments, 1 when the operation failed
   */
  public static int run(String[] arguments, BBjShipTerminal terminal) {
    return create(BBjShipPlugin.createFromBBjHome()).setTerminal(terminal).build().run(arguments);
  }

  /**
   * Runs one command.
   *
   * @param arguments the command line arguments
   *
   * @return 0 on success, 2 for invalid arguments, 1 when the operation failed
   */
  public int run(String... arguments) {
    PrintWriter out = new PrintWriter(new LineWriter(terminal::print), true);
    PrintWriter err = new PrintWriter(new LineWriter(terminal::printError), true);
    try {
      return createCommandLine(out, err).execute(arguments);
    } finally {
      out.close();
      err.close();
    }
  }

  private CommandLine createCommandLine(PrintWriter out, PrintWriter err) {
    CommandLine root = new CommandLine(new BBjShipCommandGroup());
    root.getCommandSpec().name(BBjShipCommandLine.BBJSHIP).usageMessage()
        .description("Configure webforJ BBjShip and manage the applications it deployed.")
        .footer("",
            BBjShipCommandLine.USERNAME_VARIABLE + " and " + BBjShipCommandLine.PASSWORD_VARIABLE
                + " are used when the options are "
                + "absent. Without options and variables, the BBj factory administrator account is "
                + "used.");
    CommandLine remote = new CommandLine(new BBjShipCommandGroup());
    remote.getCommandSpec().usageMessage().description("Manage the BBjShip HTTP endpoint.");
    remote.addSubcommand(new RemoteEnableCommand(session));
    remote.addSubcommand(new RemoteDisableCommand(session));
    remote.addSubcommand(new RemoteStatusCommand(session));
    root.addSubcommand(BBjShipCommandLine.REMOTE, remote);
    CommandLine development = new CommandLine(new BBjShipCommandGroup());
    development.getCommandSpec().usageMessage()
        .description("Manage development runs on this BBj machine.");
    development.addSubcommand(new DevelopmentEnableCommand(session));
    development.addSubcommand(new DevelopmentDisableCommand(session));
    root.addSubcommand(BBjShipCommandLine.DEVELOPMENT, development);
    CommandLine services = new CommandLine(new BBjShipCommandGroup());
    services.getCommandSpec().usageMessage().description("Manage the local BBjServices.")
        .hidden(true);
    services.addSubcommand(new ServicesStopCommand(session));
    root.addSubcommand(BBjShipCommandLine.SERVICES, services);
    CommandLine apps = new CommandLine(new BBjShipCommandGroup());
    apps.getCommandSpec().usageMessage()
        .description("Deploy, list and undeploy the applications BBjShip deployed.");
    apps.addSubcommand(new AppsDeployCommand(session));
    apps.addSubcommand(new AppsListCommand(session));
    apps.addSubcommand(new AppsUndeployCommand(session));
    root.addSubcommand(BBjShipCommandLine.APPS, apps);
    root.addSubcommand(new ResetCommand(session));
    root.registerConverter(Path.class,
        value -> Path.of(terminal.getWorkingDirectory()).resolve(value));
    applyVersion(root);
    BBjShipFailureHandler failures = new BBjShipFailureHandler();
    root.setOut(out).setErr(err).setColorScheme(Help.defaultColorScheme(ansi))
        .setParameterExceptionHandler(failures).setExecutionExceptionHandler(failures);

    return root;
  }

  private static Ansi resolveAnsi(BBjShipTerminal terminal) {
    String type = terminal.getVariable("TERM");
    boolean colors =
        type != null && !"dumb".equals(type) && terminal.getVariable("NO_COLOR") == null;

    return colors ? Ansi.ON : Ansi.OFF;
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

    private final BBjShipPlugin plugin;
    private BBjShipTerminal terminal;
    private Ansi ansi;
    private AdminSessionFactory sessions = new BBjServicesAdminSessionFactory();
    private Duration answerDeadline;
    private String operatingSystem;

    private Builder(BBjShipPlugin plugin) {
      this.plugin = Objects.requireNonNull(plugin);
    }

    /**
     * Sets the terminal that runs the command.
     *
     * @param terminal the terminal that runs the command
     *
     * @return this builder
     */
    public Builder setTerminal(BBjShipTerminal terminal) {
      this.terminal = Objects.requireNonNull(terminal);
      return this;
    }

    /**
     * Sets the color mode. By default colors are on when the terminal sets TERM to a value other
     * than dumb and does not set NO_COLOR.
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
     * Sets the factory that opens the administration session.
     *
     * @param sessions the factory that opens the administration session
     *
     * @return this builder
     */
    public Builder setSessions(AdminSessionFactory sessions) {
      this.sessions = Objects.requireNonNull(sessions);
      return this;
    }

    /**
     * Sets the longest wait for the BBjServices web server to answer on a port of a context.
     *
     * @param answerDeadline the longest wait
     *
     * @return this builder
     */
    public Builder setAnswerDeadline(Duration answerDeadline) {
      this.answerDeadline = Objects.requireNonNull(answerDeadline);
      return this;
    }

    /**
     * Sets the name of the operating system the command runs on. By default the name the Java
     * runtime reports.
     *
     * @param operatingSystem the name of the operating system
     *
     * @return this builder
     */
    public Builder setOperatingSystem(String operatingSystem) {
      this.operatingSystem = Objects.requireNonNull(operatingSystem);
      return this;
    }

    /**
     * Builds the command line.
     *
     * @return the command line
     */
    public BBjShipCli build() {
      Objects.requireNonNull(terminal);

      return new BBjShipCli(this);
    }
  }

  /**
   * Hands every line written to it to a terminal.
   *
   * @author Hyyan Abo Fakher
   * @since 26.03
   */
  private static final class LineWriter extends Writer {

    private final Consumer<String> lines;
    private final StringBuilder pending = new StringBuilder();

    private LineWriter(Consumer<String> lines) {
      this.lines = lines;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void write(char[] buffer, int offset, int length) {
      for (int index = offset; index < offset + length; index++) {
        char next = buffer[index];
        if (next == '\n') {
          int end = pending.length();
          if (end > 0 && pending.charAt(end - 1) == '\r') {
            pending.setLength(end - 1);
          }

          lines.accept(pending.toString());
          pending.setLength(0);
        } else {
          pending.append(next);
        }
      }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void flush() {
      if (!pending.isEmpty()) {
        lines.accept(pending.toString());
        pending.setLength(0);
      }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void close() {
      flush();
    }
  }
}
