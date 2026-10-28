package com.webforj.bbjship.foundation;

/**
 * Names the commands and options of the BBjShip command line.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class BBjShipCommandLine {

  /**
   * The name of the BBjShip command line.
   */
  public static final String BBJSHIP = "bbjship";

  /**
   * The command group of the applications BBjShip deployed.
   */
  public static final String APPS = "apps";

  /**
   * The command group of the BBjShip HTTP endpoint.
   */
  public static final String REMOTE = "remote";

  /**
   * The command group of development runs.
   */
  public static final String DEVELOPMENT = "development";

  /**
   * The command group of the local BBjServices.
   */
  public static final String SERVICES = "services";

  /**
   * The command that undoes every change BBjShip made to the BBj machine.
   */
  public static final String RESET = "reset";

  /**
   * The command that deploys an application archive.
   */
  public static final String DEPLOY = "deploy";

  /**
   * The command that undeploys an application.
   */
  public static final String UNDEPLOY = "undeploy";

  /**
   * The command that lists the deployed applications.
   */
  public static final String LIST = "list";

  /**
   * The command that enables the endpoint or development runs.
   */
  public static final String ENABLE = "enable";

  /**
   * The command that disables the endpoint or development runs.
   */
  public static final String DISABLE = "disable";

  /**
   * The command that shows the addresses of the endpoint.
   */
  public static final String STATUS = "status";

  /**
   * The command that stops the local BBjServices.
   */
  public static final String STOP = "stop";

  /**
   * The option that undeploys every application.
   */
  public static final String ALL_OPTION = "--all";

  /**
   * The option that runs a reset without asking for confirmation.
   */
  public static final String YES_OPTION = "--yes";

  /**
   * The option that adds the deployment folders and class folders to the application list.
   */
  public static final String LONG_OPTION = "--long";

  /**
   * The short name of {@link #LONG_OPTION}.
   */
  public static final String LONG_SHORT_OPTION = "-l";

  /**
   * The option naming the properties file of BBjServices.
   */
  public static final String PROPERTIES_OPTION = "--properties";

  /**
   * The option naming the BBj administrator.
   */
  public static final String USERNAME_OPTION = "--username";

  /**
   * The option holding the password of the BBj administrator.
   */
  public static final String PASSWORD_OPTION = "--password";

  /**
   * The option that reads the password from the first line of standard input.
   */
  public static final String PASSWORD_STDIN_OPTION = "--password-stdin";

  /**
   * The option naming the HTTP port of the BBjServices web server.
   */
  public static final String PORT_OPTION = "--port";

  /**
   * The option naming the SSL port of the BBjServices web server.
   */
  public static final String SSL_PORT_OPTION = "--ssl-port";

  /**
   * The option that allows unencrypted requests from another machine.
   */
  public static final String ALLOW_UNENCRYPTED_OPTION = "--allow-unencrypted";

  /**
   * The environment variable naming the BBj administrator.
   */
  public static final String USERNAME_VARIABLE = "WEBFORJ_BBJ_USERNAME";

  /**
   * The environment variable holding the password of the BBj administrator.
   */
  public static final String PASSWORD_VARIABLE = "WEBFORJ_BBJ_PASSWORD";

  private BBjShipCommandLine() {}
}
