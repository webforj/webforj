package com.webforj.bbjship.cli.command;

import com.basis.api.admin.BBjAdminBase;
import com.basis.api.admin.BBjAdminException;
import com.webforj.bbjship.cli.admin.BBjShipServices;
import com.webforj.bbjship.cli.admin.BBjShipSession;
import com.webforj.bbjship.cli.option.CredentialOptions;
import java.io.IOException;
import java.util.List;
import java.util.OptionalInt;
import java.util.OptionalLong;
import java.util.concurrent.Callable;
import picocli.CommandLine;
import picocli.CommandLine.Command;
import picocli.CommandLine.Help.Ansi.Style;
import picocli.CommandLine.Mixin;
import picocli.CommandLine.Model.CommandSpec;
import picocli.CommandLine.Spec;

/**
 * Restarts the BBjServices of this machine, or starts it when it is not running.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
@Command(name = "restart", mixinStandardHelpOptions = true,
    description = "Restart the local BBjServices.")
public final class ServicesRestartCommand implements Callable<Integer> {

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
  public ServicesRestartCommand(BBjShipSession session) {
    this.session = session;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Integer call() throws Exception {
    BBjShipServices services = session.getServices();
    String action;
    if (services.isRunning()) {
      BBjAdminBase admin = session.openAdmin(credentials, OptionalInt.empty(), OptionalInt.empty());
      OptionalLong processId;
      try {
        processId = services.readProcessId(admin, session.getCredentials());
      } catch (IOException | BBjAdminException | InterruptedException | RuntimeException failure) {
        admin.release();
        throw failure;
      }

      services.setLog(spec.commandLine().getOut()::println);
      services.restart(admin, processId);
      action = "Restarted";
    } else {
      services.start();
      action = "Started";
    }

    CommandLine line = spec.commandLine();
    line.getOut().println(
        line.getColorScheme().apply(action, List.of(Style.fg_green)).concat(" BBjServices"));

    return 0;
  }
}
