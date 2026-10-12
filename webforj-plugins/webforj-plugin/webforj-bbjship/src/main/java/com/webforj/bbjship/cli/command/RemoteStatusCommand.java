package com.webforj.bbjship.cli.command;

import com.basis.api.admin.BBjAdminBase;
import com.webforj.bbjship.cli.admin.BBjShipContexts;
import com.webforj.bbjship.cli.admin.BBjShipSession;
import com.webforj.bbjship.cli.option.CredentialOptions;
import java.net.URI;
import java.nio.file.Files;
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
 * Shows whether the BBjShip deployment endpoint is enabled and its address, and whether development
 * runs are enabled.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
@Command(name = "status", mixinStandardHelpOptions = true,
    description = "Show the BBjShip HTTP endpoint.")
public final class RemoteStatusCommand implements Callable<Integer> {

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
  public RemoteStatusCommand(BBjShipSession session) {
    this.session = session;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Integer call() throws Exception {
    BBjAdminBase admin = session.openAdmin(credentials, OptionalInt.empty(), OptionalInt.empty());
    List<URI> addresses;
    try {
      addresses = new BBjShipContexts(admin, session.getPlugin()).getAddresses();
    } finally {
      admin.release();
    }

    CommandLine line = spec.commandLine();
    if (addresses.isEmpty()) {
      line.getOut().println("The BBjShip HTTP endpoint is not enabled");
    }

    addresses.forEach(address -> line.getOut().println(
        line.getColorScheme().apply("Enabled", List.of(Style.fg_green)).concat(" " + address)));
    boolean development = Files.isRegularFile(session.getPlugin().getDevelopmentSwitch());
    line.getOut()
        .println(line.getColorScheme().text("Development runs  ")
            .concat(development ? line.getColorScheme().apply("enabled", List.of(Style.fg_green))
                : line.getColorScheme().text("disabled")));

    return 0;
  }
}
