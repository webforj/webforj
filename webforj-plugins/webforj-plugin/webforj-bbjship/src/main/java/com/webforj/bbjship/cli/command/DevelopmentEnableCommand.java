package com.webforj.bbjship.cli.command;

import com.basis.api.admin.BBjAdminBase;
import com.webforj.bbjship.cli.admin.BBjShipSession;
import com.webforj.bbjship.cli.option.CredentialOptions;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.PosixFilePermissions;
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
 * Enables development runs, deployments that load the application from class folders on this
 * machine.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
@Command(name = "enable", mixinStandardHelpOptions = true,
    description = "Enable development runs on this BBj machine.")
public final class DevelopmentEnableCommand implements Callable<Integer> {

  private static final String CONTENT = "development runs are enabled on this machine";

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
  public DevelopmentEnableCommand(BBjShipSession session) {
    this.session = session;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Integer call() throws Exception {
    BBjAdminBase admin = session.openAdmin(credentials, OptionalInt.empty(), OptionalInt.empty());
    Path file = session.getPlugin().getDevelopmentSwitch();
    try {
      Files.writeString(file, CONTENT);
      if (file.getFileSystem().supportedFileAttributeViews().contains("posix")) {
        Files.setPosixFilePermissions(file, PosixFilePermissions.fromString("rw-------"));
      }
    } finally {
      admin.release();
    }

    CommandLine line = spec.commandLine();
    line.getOut().println(line.getColorScheme().apply("Enabled", List.of(Style.fg_green))
        .concat(" development runs"));

    return 0;
  }
}
