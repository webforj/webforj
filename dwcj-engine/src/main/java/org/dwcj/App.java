package org.dwcj;

import com.basis.startup.type.BBjException;

import org.dwcj.annotation.AnnotationProcessor;
import org.dwcj.bridge.IDwcjBBjBridge;
import org.dwcj.environment.namespace.*;
import org.dwcj.exceptions.DwcAppInitializeException;
import org.dwcj.exceptions.DwcException;
import org.dwcj.exceptions.DwcRuntimeException;

/**
 * This is the central class representing an app. In order to implement an app,
 * extend this class and
 * override the run() method.
 *
 */
@SuppressWarnings("java:S1610") // we want this to be abstract class, not interface
public abstract class App {

  /**
   * An enum for the default application themes
   *
   * @see App#setTheme(Theme)
   */
  public enum Theme {
    LIGHT("light"),
    DARK("dark"),
    DARK_PURE("dark-pure"),
    SYSTEM("system");

    private String value;

    private Theme(String value) {
      this.value = value;
    }

    public String getValue() {
      return value;
    }
  }

  protected App() {
    preRun();
    try {
      AnnotationProcessor processor = new AnnotationProcessor();
      processor.processAppAnnotations(this, AnnotationProcessor.RunningPhase.PRE_RUN);
      run();
      processor.processAppAnnotations(this, AnnotationProcessor.RunningPhase.POST_RUN);
    } catch (DwcException e) {
      Environment.logError(e);
    }
  }

  /**
   * Get the current page instance
   * 
   * @return the current page instance
   */
  public static Page getPage() {
    return Page.getInstance();
  }

  /**
   * Set the application theme
   *
   * @param theme The theme to set
   * @throws DwcRuntimeException if failed to set the theme
   */
  public static void setTheme(String theme) {
    try {
      Environment.getInstance().getBBjAPI().getWebManager().setTheme(theme);
    } catch (BBjException e) {
      throw new DwcRuntimeException("Failed to set theme.", e);
    }
  }

  /**
   * Set the application theme
   *
   * @param theme The theme to set
   * @throws DwcRuntimeException if failed to set the theme
   *
   * @see Theme
   */
  public static void setTheme(Theme theme) {
    setTheme(theme.getValue());
  }

  /**
   * Get the application theme
   *
   * @return The theme
   * @throws DwcRuntimeException if failed to get the theme
   */
  public static String getTheme() {
    try {
      return Environment.getInstance().getBBjAPI().getWebManager().getTheme();
    } catch (BBjException e) {
      throw new DwcRuntimeException("Failed to get theme.", e);
    }
  }

  /**
   * Set the name of the dark theme to use for the application.
   * The dark theme setting is used when the application theme is set to "system".
   *
   * @param darkTheme The dark theme to set
   * @throws DwcRuntimeException if failed to set the dark theme
   */
  public static void setDarkTheme(String darkTheme) {
    try {
      Environment.getInstance().getBBjAPI().getWebManager().setDarkTheme(darkTheme);
    } catch (BBjException e) {
      throw new DwcRuntimeException("Failed to set dark theme.", e);
    }
  }

  /**
   * Get the name of the dark theme
   *
   * @return The dark theme
   * @throws DwcRuntimeException if failed to get the dark theme
   */
  public static String getDarkTheme() {
    try {
      return Environment.getInstance().getBBjAPI().getWebManager().getDarkTheme();
    } catch (BBjException e) {
      throw new DwcRuntimeException("Failed to get dark theme.", e);
    }
  }

  /**
   * Set the name of the light theme to use for the application.
   * The light theme setting is used when the application theme is set to
   * "system".
   *
   * @param lightTheme The light theme to set
   * @throws DwcRuntimeException if failed to set the light theme
   */

  public static void setLightTheme(String lightTheme) {
    try {
      Environment.getInstance().getBBjAPI().getWebManager().setLightTheme(lightTheme);
    } catch (BBjException e) {
      throw new DwcRuntimeException("Failed to set light theme.", e);
    }
  }

  /**
   * Get the name of the light theme to use for the application.
   *
   * @return The light theme
   * @throws DwcRuntimeException if failed to get the light theme
   */

  public static String getLightTheme() {
    try {
      return Environment.getInstance().getBBjAPI().getWebManager().getLightTheme();
    } catch (BBjException e) {
      throw new DwcRuntimeException("Failed to get light theme.", e);
    }
  }

  /**
   * Get the registered DWC application name
   *
   * @return the application name
   * @throws DwcRuntimeException if failed to get the application name
   */
  public static String getApplicationName() {
    try {
      return Environment.getInstance().getBBjAPI().getWebManager().getApplicationName();
    } catch (BBjException e) {
      throw new DwcRuntimeException("Failed to get application name.", e);
    }
  }

  /**
   * Get the application protocol
   * 
   * @return The application protocol
   */
  public static String getProtocol() {
    IDwcjBBjBridge helper = Environment.getInstance().getDwcjHelper();
    Object instance = helper.createInstance("::BBUtils.bbj::BBUtils");

    return (String) helper.invokeMethod(instance, "getWebServerProtocol", null);
  }

  /**
   * Get the application host
   * 
   * @return The application host
   */
  public static String getHost() {
    IDwcjBBjBridge helper = Environment.getInstance().getDwcjHelper();
    Object instance = helper.createInstance("::BBUtils.bbj::BBUtils");

    return (String) helper.invokeMethod(instance, "getWebServerHost", null);
  }

  /**
   * Get the application port
   * 
   * @return The application port
   */
  public static String getPort() {
    IDwcjBBjBridge helper = Environment.getInstance().getDwcjHelper();
    Object instance = helper.createInstance("::BBUtils.bbj::BBUtils");

    return (String) helper.invokeMethod(instance, "getWebServerPort", null);
  }

  /**
   * Get the application URL
   * 
   * @return The application URL
   * @throws DwcRuntimeException if failed to get the application URL
   */
  public static String getUrl() {
    try {
      return Environment.getInstance().getBBjAPI().getWebManager().getUrl();
    } catch (BBjException e) {
      throw new DwcRuntimeException("Failed to get application URL.", e);
    }
  }

  /**
   * Log a String to the browser console (console.out)
   *
   * @param output The message to log
   */
  public static void consoleLog(String output) {
    try {

      Environment.getInstance().getSysGui().executeScript("console.log(\"" + output + "\")");
    } catch (BBjException e) {
      Environment.logError(e);
    }
  }

  public static void consoleError(String output) {
    try {

      Environment.getInstance().getSysGui().executeScript("console.error(\"" + output + "\")");
    } catch (BBjException e) {
      Environment.logError(e);
    }
  }

  /**
   * Shows a message box
   *
   * @param alert The message to show
   * @return
   */
  public static int msgbox(String alert) {
    return Environment.getInstance().getDwcjHelper().msgbox(alert, 0, "");
  }

  /**
   *
   * @param alert   The message to show
   * @param options
   * @return
   */
  public static int msgbox(String alert, int options) {
    return Environment.getInstance().getDwcjHelper().msgbox(alert, options, "");
  }

  /**
   *
   * @param alert   The message to show
   * @param options
   * @param title
   * @return
   */
  public static int msgbox(String alert, int options, String title) {
    return Environment.getInstance().getDwcjHelper().msgbox(alert, options, title);
  }

  /**
   * Show or hide a busy indicator overlay
   *
   * @param busy A boolean value true=show false=hide
   */
  public static void busy(boolean busy) {
    try {
      if (busy) {
        Environment.getInstance().getBBjAPI().getBuiManager().getBusyIndicator().setText("");
      }
      Environment.getInstance().getBBjAPI().getBuiManager().getBusyIndicator().setVisible(busy);
    } catch (BBjException e) {
      // ignore
    }
  }

  /**
   * show the busy indicator with the text passed to this method
   *
   * @param busyText the text to show
   */
  public static void busy(String busyText) {
    try {
      Environment.getInstance().getBBjAPI().getBuiManager().getBusyIndicator().setText(busyText);
      Environment.getInstance().getBBjAPI().getBuiManager().getBusyIndicator().setVisible(true);
    } catch (BBjException e) {
      // ignore
    }
  }

  private void preRun() {
    Environment.getInstance().getBBjAPI().setCustomEventCallback("doTerminate", "terminate");
  }

  /**
   * Call this method to terminate your App.
   */
  public void terminate() {
    Environment.getInstance().getBBjAPI().postPriorityCustomEvent("doTerminate", null);
    cleanup();
    Environment.cleanup();
  }

  /**
   * Override this method to implement custom cleanup
   * e.g. kill all background threads that may still run
   */
  public void cleanup() {
  }

  /**
   * Override this method to implement your app behavior
   *
   * @throws DwcAppInitializeException
   */
  public abstract void run() throws DwcException;

  public static Namespace getNamespace(Namespace.NamespaceType namespaceType) {
    switch (namespaceType) {
      case PRIVATE:
        throw new IllegalArgumentException("PRIVATE namespaces have a prefix and a name!");
      case GROUP:
        return new GroupNamespace();
      case GLOBAL:
        return new GlobalNamespace();
      default:
        throw new IllegalArgumentException("Illegal Type!");

    }
  }

  public static Namespace getNamespace(String prefix, String name, Boolean fCreateIfMissing) {
    if (prefix.isBlank() || name.isBlank())
      throw new IllegalArgumentException("You need a prefix and a name here");

    return new PrivateNamespace(prefix, name, fCreateIfMissing);

  }
}
