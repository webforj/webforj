package com.webforj.bbjship.cli;

import com.webforj.plugin.foundation.bbj.BBjShipCommandLine;
import java.util.ArrayDeque;
import java.util.Deque;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Stream;
import picocli.CommandLine;
import picocli.CommandLine.IExecutionExceptionHandler;
import picocli.CommandLine.IParameterExceptionHandler;
import picocli.CommandLine.MissingParameterException;
import picocli.CommandLine.Model.ArgGroupSpec;
import picocli.CommandLine.Model.ArgSpec;
import picocli.CommandLine.Model.CommandSpec;
import picocli.CommandLine.Model.OptionSpec;
import picocli.CommandLine.Model.PositionalParamSpec;
import picocli.CommandLine.MutuallyExclusiveArgsException;
import picocli.CommandLine.OverwrittenOptionException;
import picocli.CommandLine.ParameterException;
import picocli.CommandLine.ParseResult;
import picocli.CommandLine.UnmatchedArgumentException;

/**
 * Reports invalid arguments and failed operations of the BBjShip commands.
 *
 * <p>
 * An invalid argument ends the command with exit code 2, a failed operation with exit code 1. Both
 * print one message to the error stream.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class BBjShipFailureHandler
    implements IParameterExceptionHandler, IExecutionExceptionHandler {

  private static final Map<String, String> MISSING_VALUES =
      Map.of(BBjShipCommandLine.USERNAME_OPTION,
          "Specify a user name after " + BBjShipCommandLine.USERNAME_OPTION,
          BBjShipCommandLine.PORT_OPTION, "Specify a port after " + BBjShipCommandLine.PORT_OPTION,
          BBjShipCommandLine.SSL_PORT_OPTION,
          "Specify a port after " + BBjShipCommandLine.SSL_PORT_OPTION, "ARCHIVE",
          "Specify the path of a deployment archive ending in .zip");
  private static final Map<String, String> TARGETS = Map.of("NAME", "an application name");
  private static final Map<String, String> SINGLE_VALUES =
      Map.of("NAME", "Specify one application name", "ARCHIVE", "Specify one deployment archive");

  /**
   * {@inheritDoc}
   */
  @Override
  public int handleParseException(ParameterException failure, String[] arguments) {
    CommandLine line = failure.getCommandLine();
    line.getErr().println(line.getColorScheme().errorText(formatMessage(failure)));

    return line.getCommandSpec().exitCodeOnInvalidInput();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int handleExecutionException(Exception failure, CommandLine line, ParseResult result) {
    String detail = failure.getMessage() == null ? failure.toString() : failure.getMessage();
    line.getErr().println(line.getColorScheme().errorText("Command failed. " + detail));

    return line.getCommandSpec().exitCodeOnExecutionException();
  }

  /**
   * Refuses an option or parameter value that is empty or looks like an option.
   *
   * @param spec the command that owns the option or parameter
   * @param option the option name or the label of the parameter
   * @param value the value given on the command line
   *
   * @throws ParameterException if the value is empty or starts with a dash
   */
  public static void applyValueValidation(CommandSpec spec, String option, String value) {
    if (value.isEmpty() || value.startsWith("-")) {
      throw createMissingValueException(spec, option);
    }
  }

  /**
   * Creates the refusal of an option or parameter that has no usable value.
   *
   * @param spec the command that owns the option or parameter
   * @param option the option name or the label of the parameter
   *
   * @return the refusal that asks for the value
   */
  public static ParameterException createMissingValueException(CommandSpec spec, String option) {
    return new ParameterException(spec.commandLine(), MISSING_VALUES.get(option));
  }

  private static String formatMessage(ParameterException failure) {
    if (failure instanceof OverwrittenOptionException overwritten) {
      return "Specify " + ((OptionSpec) overwritten.getOverwritten()).longestName() + " only once";
    }

    if (failure instanceof MutuallyExclusiveArgsException) {
      return formatTarget(failure.getCommandLine().getCommandSpec().argGroups().get(0));
    }

    if (failure instanceof MissingParameterException missing) {
      return formatMissing(missing.getMissing().get(0));
    }

    if (failure instanceof UnmatchedArgumentException unmatched) {
      return formatUnmatched(failure.getCommandLine().getCommandSpec(),
          unmatched.getUnmatched().get(0));
    }

    return failure.getMessage();
  }

  private static String formatMissing(ArgSpec argument) {
    if (argument.group() != null) {
      return formatTarget(argument.group());
    }

    String key =
        argument instanceof OptionSpec option ? option.longestName() : argument.paramLabel();

    return MISSING_VALUES.get(key);
  }

  private static String formatTarget(ArgGroupSpec group) {
    return "Specify " + TARGETS.get(group.positionalParameters().get(0).paramLabel()) + " or "
        + group.options().get(0).longestName();
  }

  private static String formatUnmatched(CommandSpec spec, String argument) {
    String command = toCommandName(spec);
    if (argument.startsWith("-")) {
      if (hasOption(spec.root(), argument)) {
        return "Option " + argument + " is not available for "
            + (command.isEmpty() ? spec.root().name() : command)
            + ". Use --help for available options";
      }

      return "Unknown option " + argument + ". Use --help for available options";
    }

    Optional<PositionalParamSpec> positional = findPositional(spec);
    if (positional.isPresent()) {
      return SINGLE_VALUES.get(positional.get().paramLabel());
    }

    return "Unknown command " + (command + " " + argument).strip()
        + ". Use --help for available commands";
  }

  private static Optional<PositionalParamSpec> findPositional(CommandSpec spec) {
    return Stream
        .concat(spec.positionalParameters().stream(),
            spec.argGroups().stream().flatMap(group -> group.positionalParameters().stream()))
        .findFirst();
  }

  private static boolean hasOption(CommandSpec root, String name) {
    Deque<CommandSpec> pending = new ArrayDeque<>(List.of(root));
    while (!pending.isEmpty()) {
      CommandSpec spec = pending.pop();
      if (spec.findOption(name) != null) {
        return true;
      }

      spec.subcommands().values().forEach(line -> pending.push(line.getCommandSpec()));
    }

    return false;
  }

  private static String toCommandName(CommandSpec spec) {
    String qualified = spec.qualifiedName(" ");
    String root = spec.root().name();

    return qualified.equals(root) ? "" : qualified.substring(root.length() + 1);
  }
}
