package com.webforj.bbjship.cli.command;

import java.util.concurrent.Callable;
import picocli.CommandLine.Command;
import picocli.CommandLine.Model.CommandSpec;
import picocli.CommandLine.ParameterException;
import picocli.CommandLine.Spec;

/**
 * Groups BBjShip commands under one name and shows their usage when no command is given.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
@Command(mixinStandardHelpOptions = true, synopsisSubcommandLabel = "COMMAND")
public final class BBjShipCommandGroup implements Callable<Integer> {

  @Spec
  private CommandSpec spec;

  /**
   * {@inheritDoc}
   */
  @Override
  public Integer call() {
    spec.commandLine().usage(spec.commandLine().getOut());

    throw new ParameterException(spec.commandLine(),
        "Specify a command. Use --help for available commands");
  }
}
