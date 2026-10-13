package com.webforj.bbjship.cli.command;

import com.basis.api.admin.BBjAdminBase;
import com.webforj.bbjship.cli.admin.BBjShipSession;
import com.webforj.bbjship.cli.option.CredentialOptions;
import com.webforj.plugin.foundation.bbj.BBjShipCommandLine;
import java.util.List;
import java.util.OptionalInt;
import java.util.concurrent.Callable;
import picocli.CommandLine;
import picocli.CommandLine.Command;
import picocli.CommandLine.Help.Ansi.Style;
import picocli.CommandLine.Mixin;
import picocli.CommandLine.Model.CommandSpec;
import picocli.CommandLine.Spec;

/**
 * Asks the BBjServices of this machine to shut down.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
@Command(name = BBjShipCommandLine.STOP, mixinStandardHelpOptions = true,
    description = "Stop the local BBjServices.")
public final class ServicesStopCommand implements Callable<Integer> {

  private final BBjShipSession session;

  @Spec
  private CommandSpec spec;

  @Mixin
  private CredentialOptions credentials;

  /**
   * Creates the command.
   *
   * @param session the session of the BBjShip command line
   */
  public ServicesStopCommand(BBjShipSession session) {
    this.session = session;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Integer call() throws Exception {
    CommandLine line = spec.commandLine();
    BBjAdminBase admin = session.openAdmin(credentials, OptionalInt.empty(), OptionalInt.empty());
    try {
      line.getOut().println(
          line.getColorScheme().apply("Stopping", List.of(Style.fg_green)).concat(" BBjServices"));
      admin.shutdownBBjServices(false);
    } finally {
      admin.release();
    }

    return 0;
  }
}
