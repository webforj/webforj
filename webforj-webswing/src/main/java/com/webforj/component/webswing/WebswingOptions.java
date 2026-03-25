package com.webforj.component.webswing;

/**
 * Configuration options for the Webswing instance initialization. These options are passed directly
 * to the Webswing API bootstrap method.
 *
 * @author Hyyan Abo Fakher
 * @since 25.10
 */
public class WebswingOptions {
  private boolean autoStart = false;
  private String securityToken = null;
  private String realm = null;
  private String args = null;
  private boolean recording = false;
  private String connectionUrl = null;
  private Integer debugPort = null;

  /**
   * Sets whether Webswing should execute the {@code start()} method right after the instance is
   * initialized.
   *
   * @param autoStart whether autoStart is enabled
   * @return the options object
   */
  public WebswingOptions setAutoStart(boolean autoStart) {
    this.autoStart = autoStart;
    return this;
  }

  /**
   * Gets whether Webswing should execute the {@code start()} method right after the instance is
   * initialized.
   *
   * @return whether autoStart is enabled
   */
  public boolean isAutoStart() {
    return autoStart;
  }

  /**
   * Sets the parameter passed to the security module during authentication.
   *
   * @param securityToken the security token
   * @return the options object
   */
  public WebswingOptions setSecurityToken(String securityToken) {
    this.securityToken = securityToken;
    return this;
  }

  /**
   * Gets the parameter passed to the security module during authentication.
   *
   * @return the security token
   */
  public String getSecurityToken() {
    return securityToken;
  }

  /**
   * Sets the parameter passed to the security module during authentication.
   *
   * @param realm the realm
   * @return the options object
   */
  public WebswingOptions setRealm(String realm) {
    this.realm = realm;
    return this;
  }

  /**
   * Gets the parameter passed to the security module during authentication.
   *
   * @return the realm
   */
  public String getRealm() {
    return realm;
  }

  /**
   * Sets additional Java application arguments. Value of this option is available in Webswing
   * config using the ${customArgs} variable.
   *
   * @param args the additional Java application arguments
   * @return the options object
   */
  public WebswingOptions setArgs(String args) {
    this.args = args;
    return this;
  }

  /**
   * Gets additional Java application arguments. Value of this option is available in Webswing
   * config using the ${customArgs} variable.
   *
   * @return the additional Java application arguments
   */
  public String getArgs() {
    return args;
  }

  /**
   * Sets whether this application session is recorded (default: false).
   *
   * @param recording whether the session is recorded
   * @return the options object
   */
  public WebswingOptions setRecording(boolean recording) {
    this.recording = recording;
    return this;
  }

  /**
   * Gets whether this application session is recorded (default: false).
   *
   * @return whether the session is recorded
   */
  public boolean isRecording() {
    return recording;
  }

  /**
   * Sets the connection URL override. If not set, the URL from the connector will be used.
   *
   * @param connectionUrl the connection URL
   * @return the options object
   */
  public WebswingOptions setConnectionUrl(String connectionUrl) {
    this.connectionUrl = connectionUrl;
    return this;
  }

  /**
   * Gets the connection URL override.
   *
   * @return the connection URL
   */
  public String getConnectionUrl() {
    return connectionUrl;
  }

  /**
   * Sets the integer that specifies on which port should the debugger listen to. See development
   * docs for more information.
   *
   * @param debugPort the debug port
   * @return the options object
   */
  public WebswingOptions setDebugPort(Integer debugPort) {
    this.debugPort = debugPort;
    return this;
  }

  /**
   * Gets the integer that specifies on which port should the debugger listen to. See development
   * docs for more information.
   *
   * @return the debug port
   */
  public Integer getDebugPort() {
    return debugPort;
  }
}
