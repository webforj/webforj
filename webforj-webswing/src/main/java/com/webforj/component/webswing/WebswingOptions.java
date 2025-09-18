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
  private Integer autoReconnect = null;
  private boolean disableLogout = false;
  private boolean disableLogin = false;
  private boolean syncClipboard = false;
  private String securityToken = null;
  private String realm = null;
  private String args = null;
  private boolean recording = false;
  private String connectionUrl = null;
  private Integer debugPort = null;
  private int javaCallTimeout = 3000;
  private PingParams pingParams = new PingParams();

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
   * Sets the number of milliseconds to wait until re-connection attempt in case of connection to
   * server is terminated.
   *
   * @param autoReconnect the number of milliseconds to wait until re-connection attempt
   * @return the options object
   */
  public WebswingOptions setAutoReconnect(Integer autoReconnect) {
    this.autoReconnect = autoReconnect;
    return this;
  }

  /**
   * Gets the number of milliseconds to wait until re-connection attempt in case of connection to
   * server is terminated.
   *
   * @return the number of milliseconds to wait until re-connection attempt
   */
  public Integer getAutoReconnect() {
    return autoReconnect;
  }

  /**
   * Sets whether the Logout button is removed from all dialogs.
   *
   * @param disableLogout whether the Logout button is removed
   * @return the options object
   */
  public WebswingOptions setDisableLogout(boolean disableLogout) {
    this.disableLogout = disableLogout;
    return this;
  }

  /**
   * Gets whether the Logout button is removed from all dialogs.
   *
   * @return whether the Logout button is removed
   */
  public boolean isDisableLogout() {
    return disableLogout;
  }

  /**
   * Sets whether the login process is completely disabled.
   *
   * @param disableLogin whether the login process is disabled
   * @return the options object
   */
  public WebswingOptions setDisableLogin(boolean disableLogin) {
    this.disableLogin = disableLogin;
    return this;
  }

  /**
   * Gets whether the login process is completely disabled.
   *
   * @return whether the login process is disabled
   */
  public boolean isDisableLogin() {
    return disableLogin;
  }

  /**
   * Sets whether synchronization of user's local clipboard with Webswing is enabled (not yet
   * supported in Firefox).
   *
   * @param syncClipboard whether clipboard synchronization is enabled
   * @return the options object
   */
  public WebswingOptions setSyncClipboard(boolean syncClipboard) {
    this.syncClipboard = syncClipboard;
    return this;
  }

  /**
   * Gets whether synchronization of user's local clipboard with Webswing is enabled (not yet
   * supported in Firefox).
   *
   * @return whether clipboard synchronization is enabled
   */
  public boolean isSyncClipboard() {
    return syncClipboard;
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

  /**
   * Sets the JsLink Java method invocation timeout. If Java method is not returned with a result,
   * an error is logged to the browser console.
   *
   * @param javaCallTimeout the Java method invocation timeout
   * @return the options object
   */
  public WebswingOptions setJavaCallTimeout(int javaCallTimeout) {
    this.javaCallTimeout = javaCallTimeout;
    return this;
  }

  /**
   * Gets the JsLink Java method invocation timeout. If Java method is not returned with a result,
   * an error is logged to the browser console.
   *
   * @return the Java method invocation timeout
   */
  public int getJavaCallTimeout() {
    return javaCallTimeout;
  }

  /**
   * Sets the setup parameters for checking connection stability.
   *
   * @param pingParams the ping parameters
   * @return the options object
   */
  public WebswingOptions setPingParams(PingParams pingParams) {
    this.pingParams = pingParams;
    return this;
  }

  /**
   * Gets the setup parameters for checking connection stability.
   *
   * @return the ping parameters
   */
  public PingParams getPingParams() {
    return pingParams;
  }

  /**
   * Setup parameters for checking connection stability.
   */
  public static class PingParams {
    private int count = 6;
    private int interval = 5;
    private int maxLatency = 500;
    private int notifyIf = 3;
    private String url = null;

    /**
     * Sets the number of ping attempts.
     *
     * @param count the number of ping attempts
     * @return the options object
     */
    public PingParams setCount(int count) {
      this.count = count;
      return this;
    }

    /**
     * Gets the number of ping attempts.
     *
     * @return the number of ping attempts
     */
    public int getCount() {
      return count;
    }

    /**
     * Sets the interval between ping attempts in seconds.
     *
     * @param interval the interval between ping attempts
     * @return the options object
     */
    public PingParams setInterval(int interval) {
      this.interval = interval;
      return this;
    }

    /**
     * Gets the interval between ping attempts in seconds.
     *
     * @return the interval between ping attempts
     */
    public int getInterval() {
      return interval;
    }

    /**
     * Sets the maximum acceptable latency in milliseconds.
     *
     * @param maxLatency the maximum acceptable latency
     * @return the options object
     */
    public PingParams setMaxLatency(int maxLatency) {
      this.maxLatency = maxLatency;
      return this;
    }

    /**
     * Gets the maximum acceptable latency in milliseconds.
     *
     * @return the maximum acceptable latency
     */
    public int getMaxLatency() {
      return maxLatency;
    }

    /**
     * Sets the value in milliseconds to notify if latency exceeds.
     *
     * @param notifyIf the value to notify if latency exceeds
     * @return the options object
     */
    public PingParams setNotifyIf(int notifyIf) {
      this.notifyIf = notifyIf;
      return this;
    }

    /**
     * Gets the value in milliseconds to notify if latency exceeds.
     *
     * @return the value to notify if latency exceeds
     */
    public int getNotifyIf() {
      return notifyIf;
    }

    /**
     * Sets the custom ping URL.
     *
     * @param url the ping URL
     * @return the options object
     */
    public PingParams setUrl(String url) {
      this.url = url;
      return this;
    }

    /**
     * Gets the custom ping URL.
     *
     * @return the ping URL
     */
    public String getUrl() {
      return url;
    }
  }
}
