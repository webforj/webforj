package com.webforj.bbjship.cli.command;

import com.basis.api.admin.BBjAdminBase;
import com.webforj.bbjship.cli.admin.BBjShipSession;
import com.webforj.bbjship.cli.option.CredentialOptions;
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
 * Disables development runs on this machine.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
@Command(name = "disable", mixinStandardHelpOptions = true,
    description = "Disable development runs on this BBj machine.")
public final class DevelopmentDisableCommand implements Callable<Integer> {

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
  public DevelopmentDisableCommand(BBjShipSession session) {
    this.session = session;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Integer call() throws Exception {
    BBjAdminBase admin = session.openAdmin(credentials, OptionalInt.empty(), OptionalInt.empty());
    try {
      Files.deleteIfExists(session.getPlugin().getDevelopmentSwitch());
    } finally {
      admin.release();
    }

    CommandLine line = spec.commandLine();
    line.getOut().println(line.getColorScheme().apply("Disabled", List.of(Style.fg_green))
        .concat(" development runs"));

    return 0;
  }
}
