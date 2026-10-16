package com.webforj.bbjship.cli.command;

import com.basis.api.admin.BBjAdminBase;
import com.webforj.bbjship.cli.admin.BBjShipSession;
import com.webforj.bbjship.cli.option.CredentialOptions;
import com.webforj.bbjship.foundation.BBjShipCommandLine;
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
 * Disables the BBjShip deployment endpoint.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
@Command(name = BBjShipCommandLine.DISABLE, mixinStandardHelpOptions = true,
    description = "Disable the BBjShip HTTP endpoint.")
public final class RemoteDisableCommand implements Callable<Integer> {

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
  public RemoteDisableCommand(BBjShipSession session) {
    this.session = session;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Integer call() throws Exception {
    BBjAdminBase admin = session.openAdmin(credentials, OptionalInt.empty(), OptionalInt.empty());
    boolean disabled;
    try {
      disabled = session.createContexts(admin).removeEndpoint();
    } finally {
      admin.release();
    }

    CommandLine line = spec.commandLine();
    line.getOut()
        .println(disabled ? line.getColorScheme().apply("Disabled", List.of(Style.fg_green))
            .concat(" the BBjShip HTTP endpoint") : "The BBjShip HTTP endpoint is not enabled");

    return 0;
  }
}
