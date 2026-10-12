package com.webforj.bbjship.cli.command;

import com.basis.api.admin.BBjAdminBase;
import com.webforj.bbjship.cli.admin.BBjShipClasspaths;
import com.webforj.bbjship.cli.admin.BBjShipContexts;
import com.webforj.bbjship.cli.admin.BBjShipSession;
import com.webforj.bbjship.cli.option.CredentialOptions;
import com.webforj.bbjship.cli.option.PortOption;
import java.net.URI;
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
 * Enables the BBjShip deployment endpoint with the BBjShip library as it is on disk.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
@Command(name = "enable", mixinStandardHelpOptions = true,
    description = "Enable the BBjShip HTTP endpoint.")
public final class RemoteEnableCommand implements Callable<Integer> {

  private final BBjShipSession session;

  @Spec
  private CommandSpec spec;

  @Mixin
  private CredentialOptions credentials;

  @Mixin
  private PortOption port;

  /**
   * Creates the command.
   *
   * @param session the session of the BBjShip command line
   */
  public RemoteEnableCommand(BBjShipSession session) {
    this.session = session;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Integer call() throws Exception {
    BBjAdminBase admin =
        session.openAdmin(credentials, OptionalInt.of(port.getPort()), port.getSslPort());
    List<URI> addresses;
    try {
      new BBjShipClasspaths(admin, session.getPlugin()).resetServlet();
      BBjShipContexts contexts = new BBjShipContexts(admin, session.getPlugin());
      contexts.enable(port.getPort(), port.getSslPort(), session.isAssetsStarted());
      addresses = contexts.getAddresses();
    } finally {
      admin.release();
    }

    CommandLine line = spec.commandLine();
    addresses.forEach(address -> line.getOut().println(
        line.getColorScheme().apply("Enabled", List.of(Style.fg_green)).concat(" " + address)));

    return 0;
  }
}
