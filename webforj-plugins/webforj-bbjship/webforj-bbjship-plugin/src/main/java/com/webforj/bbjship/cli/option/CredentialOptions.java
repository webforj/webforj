package com.webforj.bbjship.cli.option;

import com.webforj.bbjship.cli.BBjShipFailureHandler;
import com.webforj.bbjship.cli.admin.BBjShipInput;
import com.webforj.bbjship.foundation.BBjShipCommandLine;
import com.webforj.bbjship.foundation.DeploymentPolicy;
import com.webforj.bbjship.foundation.DeploymentPolicy.Authentication;
import com.webforj.bbjship.foundation.model.BBjCredentials;
import java.util.List;
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
  private static final String SUPPLY = "Specify " + BBjShipCommandLine.USERNAME_OPTION + " and "
      + BBjShipCommandLine.PASSWORD_STDIN_OPTION + ", or set " + USERNAME_VARIABLE + " and "
      + PASSWORD_VARIABLE;
  private static final String PASSWORD_PROMPT =
      "Enter the password. The terminal shows it as it is typed";

  @Spec(Spec.Target.MIXEE)
  private CommandSpec spec;

  private String username;

  @Option(names = BBjShipCommandLine.PASSWORD_STDIN_OPTION,
      description = "Read the password from the first line of standard input.")
  private boolean passwordStdin;

  @Option(names = BBjShipCommandLine.PASSWORD_OPTION,
      description = "Prompt for the password. The terminal shows the password as it is typed.")
  private boolean passwordPrompt;

  @Option(names = BBjShipCommandLine.USERNAME_OPTION, paramLabel = "USER",
      description = "BBj administrator user name.")
  void setUsername(String username) {
    BBjShipFailureHandler.applyValueValidation(spec, BBjShipCommandLine.USERNAME_OPTION, username);
    this.username = username;
  }

  /**
   * Resolves the credentials from the options, the environment or the factory account, and shows a
   * warning on the error stream while the factory account is in use.
   *
   * @param input the input of the terminal that runs the command
   *
   * @return the credentials
   * @throws ParameterException if a user name or password is blank or only one of them is given
   */
  public BBjCredentials resolveCredentials(BBjShipInput input) {
    CommandLine line = spec.commandLine();
    String resolvedUsername =
        username != null ? username : input.findVariable(USERNAME_VARIABLE).orElse(null);
    String resolvedPassword = findPassword(input).orElse(null);
    try {
      return DeploymentPolicy.resolveCredentials(resolvedUsername, resolvedPassword, null,
          Authentication.PASSWORD, warning -> line.getErr()
              .println(line.getColorScheme().apply(warning, List.of(Style.fg_yellow))));
    } catch (IllegalArgumentException refusal) {
      throw new ParameterException(line, refusal.getMessage() + ". " + SUPPLY, refusal);
    }
  }

  private Optional<String> findPassword(BBjShipInput input) {
    if (passwordStdin) {
      spec.commandLine().getOut().println("Reading the password from standard input");

      return Optional.of(input.readLine().orElse(""));
    }

    if (passwordPrompt) {
      return Optional.of(input.readPassword(PASSWORD_PROMPT).orElse(""));
    }

    return input.findVariable(PASSWORD_VARIABLE);
  }
}
