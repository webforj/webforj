package com.webforj.bbjship.cli.option;

import com.webforj.bbjship.cli.BBjShipFailureHandler;
import com.webforj.plugin.foundation.bbj.BBjShipCommandLine;
import java.util.OptionalInt;
import picocli.CommandLine.Model.CommandSpec;
import picocli.CommandLine.Option;
import picocli.CommandLine.ParameterException;
import picocli.CommandLine.Spec;

/**
 * Holds the HTTP and SSL port options of the BBjShip commands that configure a web context.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class PortOption {

  @Spec(Spec.Target.MIXEE)
  private CommandSpec spec;

  private int port;
  private OptionalInt sslPort = OptionalInt.empty();

  /**
   * Returns the HTTP port of the option.
   *
   * @return the port from 1 to 65535
   */
  public int getPort() {
    return port;
  }

  /**
   * Returns the SSL port of the options.
   *
   * @return the port from 1 to 65535, or empty when it is not given
   */
  public OptionalInt getSslPort() {
    return sslPort;
  }

  @Option(names = BBjShipCommandLine.PORT_OPTION, paramLabel = "PORT", defaultValue = "8888",
      description = "HTTP port of the BBjServices web server the BBjShip contexts listen on, "
          + "from 1 to 65535, 8888 when absent.")
  void setPort(String value) {
    port = parsePort(BBjShipCommandLine.PORT_OPTION, value);
  }

  @Option(names = BBjShipCommandLine.SSL_PORT_OPTION, paramLabel = "PORT",
      description = "SSL port of the BBjServices web server the BBjShip contexts also listen "
          + "on, from 1 to 65535.")
  void setSslPort(String value) {
    sslPort = OptionalInt.of(parsePort(BBjShipCommandLine.SSL_PORT_OPTION, value));
  }

  private int parsePort(String option, String value) {
    BBjShipFailureHandler.applyValueValidation(spec, option, value);
    String range = "Specify " + option + " from 1 to 65535";
    int parsed;
    try {
      parsed = Integer.parseInt(value);
    } catch (NumberFormatException invalid) {
      throw new ParameterException(spec.commandLine(), range, invalid);
    }

    if (parsed < 1 || parsed > 65535) {
      throw new ParameterException(spec.commandLine(), range);
    }

    return parsed;
  }
}
