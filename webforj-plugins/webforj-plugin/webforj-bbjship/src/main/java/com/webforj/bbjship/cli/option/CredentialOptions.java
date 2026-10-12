package com.webforj.bbjship.cli.option;

import com.webforj.bbjship.cli.BBjShipFailureHandler;
import com.webforj.bbjship.cli.admin.BBjShipInput;
import com.webforj.plugin.foundation.bbj.model.BBjCredentials;
import java.io.IOException;
import java.io.InputStream;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import picocli.CommandLine;
import picocli.CommandLine.Help.Ansi.Style;
import picocli.CommandLine.Model.CommandSpec;
import picocli.CommandLine.Option;
import picocli.CommandLine.ParameterException;
import picocli.CommandLine.Spec;

/**
 * Holds the administrator credentials options of a BBjShip command.
 *
 * <p>
 * The user name comes from {@code --username}, then {@code WEBFORJ_BBJ_USERNAME}. The password
 * comes from {@code --password-stdin} or the {@code --password} prompt, then
 * {@code WEBFORJ_BBJ_PASSWORD}. Without a user name and without a password, the credentials are the
 * factory administrator account of BBj.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class CredentialOptions {

  private static final String USERNAME_VARIABLE = "WEBFORJ_BBJ_USERNAME";
  private static final String PASSWORD_VARIABLE = "WEBFORJ_BBJ_PASSWORD";

  @Spec(Spec.Target.MIXEE)
  private CommandSpec spec;

  private String username;

  @Option(names = "--password-stdin",
      description = "Read the password from the first line of standard input.")
  private boolean passwordStdin;

  @Option(names = "--password", interactive = true, arity = "0",
      description = "Prompt for the password without showing it.")
  private char[] password;

  @Option(names = "--username", paramLabel = "USER", description = "BBj administrator user name.")
  void setUsername(String username) {
    BBjShipFailureHandler.applyValueValidation(spec, "--username", username);
    this.username = username;
  }

  /**
   * Resolves the credentials from the options, the environment or the factory account, and shows a
   * warning on the error stream while the factory account is in use.
   *
   * @param input the standard input of the command
   * @param environment the environment variables of the command
   *
   * @return the credentials
   * @throws IOException if standard input cannot be read
   * @throws ParameterException if a user name or password is blank or only one of them is given
   */
  public BBjCredentials resolveCredentials(InputStream input, Map<String, String> environment)
      throws IOException {
    CommandLine line = spec.commandLine();
    Optional<String> resolvedUsername = Optional.ofNullable(username)
        .or(() -> Optional.ofNullable(environment.get(USERNAME_VARIABLE)));
    if (resolvedUsername.filter(String::isBlank).isPresent()) {
      throw new ParameterException(line, "Specify a user name");
    }

    Optional<String> resolvedPassword = findPassword(input, environment);
    if (resolvedPassword.filter(String::isBlank).isPresent()) {
      throw new ParameterException(line, "Specify a password");
    }

    BBjCredentials credentials = BBjCredentials.ofFactoryAccount();
    if (resolvedUsername.isPresent() || resolvedPassword.isPresent()) {
      credentials = BBjCredentials.of(
          resolvedUsername.orElseThrow(
              () -> new ParameterException(line, "Specify --username or set " + USERNAME_VARIABLE)),
          resolvedPassword.orElseThrow(() -> new ParameterException(line,
              "Specify --password or --password-stdin, or set " + PASSWORD_VARIABLE)));
    }

    if (credentials.isFactoryAccount()) {
      line.getErr().println(line.getColorScheme().apply(BBjCredentials.FACTORY_ACCOUNT_WARNING,
          List.of(Style.fg_yellow)));
    }

    return credentials;
  }

  private Optional<String> findPassword(InputStream input, Map<String, String> environment)
      throws IOException {
    if (passwordStdin) {
      spec.commandLine().getOut().println("Reading the password from standard input");
      String line = BBjShipInput.readLine(input);

      return Optional.of(line == null ? "" : line);
    }

    if (password != null) {
      return Optional.of(new String(password));
    }

    return Optional.ofNullable(environment.get(PASSWORD_VARIABLE));
  }
}
