package org.dwcj;

import com.basis.bbj.proxies.BBjBuiCloseAction;
import com.basis.bbj.proxies.BBjWebManager;
import com.basis.startup.type.BBjException;
import java.net.URL;
import org.dwcj.annotation.AnnotationProcessor;
import org.dwcj.bridge.IDwcjBBjBridge;
import org.dwcj.environment.namespace.GlobalNamespace;
import org.dwcj.environment.namespace.GroupNamespace;
import org.dwcj.environment.namespace.Namespace;
import org.dwcj.environment.namespace.PrivateNamespace;
import org.dwcj.exceptions.DwcjAppInitializeException;
import org.dwcj.exceptions.DwcjException;
import org.dwcj.exceptions.DwcjRuntimeException;
import org.dwcj.webstorage.CookieStorage;
import org.dwcj.webstorage.LocalStorage;
import org.dwcj.webstorage.SessionStorage;

/**
 * This is the central class representing an app. In order to implement an app, extend this class
 * and override the run() method.
 *
 */
@SuppressWarnings("java:S1610") // we want this to be abstract class, not interface
public abstract class App {
  /**
   * A default app action is to clear the browser and display a localized message of "Click to
   * reload application", with a link to the application when the application is terminated or error
   * is returned.
   *
   * @see DefaultAction
   */
  public static final AppCloseAction DEFAULT_ACTION = new DefaultAction();
  /**
   * An application action which reset the default action when terminated or error occurred.
   *
   * @see NoneAction
   */
  public static final AppCloseAction NONE_ACTION = new NoneAction();

  private boolean isInitialized = false;

  /**
   * This is the main entry point for the application. It is called by the framework to initialize
   * and should not be called directly.
   *
   * @throws DwcjAppInitializeException if failed to initialize the app
   */
  public final void initialize() throws DwcjAppInitializeException {
    if (isInitialized) {
      throw new DwcjAppInitializeException("App is already initialized.");
    }

    preRun();
    try {
      AnnotationProcessor processor = new AnnotationProcessor();
      processor.processAppAnnotations(this, AnnotationProcessor.RunningPhase.PRE_RUN);
      run();
      processor.processAppAnnotations(this, AnnotationProcessor.RunningPhase.POST_RUN);
      isInitialized = true;
    } catch (DwcjException e) {
      Environment.logError(e);
    }
  }

  /**
   * Get the current page instance.
   *
   * @return the current page instance
   */
  public static Page getPage() {
    return Page.getCurrent();
  }

  /**
   * Get the current request instance.
   *
   * @return the current request instance
   */
  public static Request getRequest() {
    return Request.getCurrent();
  }

  /**
   * Set the application theme.
   *
   * @param theme The theme to set
   * @throws DwcjRuntimeException if failed to set the theme
   */
  public static void setTheme(String theme) {
    try {
      Environment.getCurrent().getBBjAPI().getWebManager().setTheme(theme);
    } catch (BBjException e) {
      throw new DwcjRuntimeException("Failed to set theme.", e);
    }
  }

  /**
   * Get the application theme.
   *
   * @return The theme
   * @throws DwcjRuntimeException if failed to get the theme
   */
  public static String getTheme() {
    try {
      return Environment.getCurrent().getBBjAPI().getWebManager().getTheme();
    } catch (BBjException e) {
      throw new DwcjRuntimeException("Failed to get theme.", e);
    }
  }

  /**
   * Set the name of the dark theme to use for the application. The dark theme setting is used when
   * the application theme is set to "system".
   *
   * @param darkTheme The dark theme to set
   * @throws DwcjRuntimeException if failed to set the dark theme
   */
  public static void setDarkTheme(String darkTheme) {
    try {
      Environment.getCurrent().getBBjAPI().getWebManager().setDarkTheme(darkTheme);
    } catch (BBjException e) {
      throw new DwcjRuntimeException("Failed to set dark theme.", e);
    }
  }

  /**
   * Get the name of the dark theme.
   *
   * @return The dark theme
   * @throws DwcjRuntimeException if failed to get the dark theme
   */
  public static String getDarkTheme() {
    try {
      return Environment.getCurrent().getBBjAPI().getWebManager().getDarkTheme();
    } catch (BBjException e) {
      throw new DwcjRuntimeException("Failed to get dark theme.", e);
    }
  }

  /**
   * Set the name of the light theme to use for the application. The light theme setting is used
   * when the application theme is set to "system".
   *
   * @param lightTheme The light theme to set
   * @throws DwcjRuntimeException if failed to set the light theme
   */

  public static void setLightTheme(String lightTheme) {
    try {
      Environment.getCurrent().getBBjAPI().getWebManager().setLightTheme(lightTheme);
    } catch (BBjException e) {
      throw new DwcjRuntimeException("Failed to set light theme.", e);
    }
  }

  /**
   * Get the name of the light theme to use for the application.
   *
   * @return The light theme
   * @throws DwcjRuntimeException if failed to get the light theme
   */

  public static String getLightTheme() {
    try {
      return Environment.getCurrent().getBBjAPI().getWebManager().getLightTheme();
    } catch (BBjException e) {
      throw new DwcjRuntimeException("Failed to get light theme.", e);
    }
  }

  /**
   * Get the registered DWC application name.
   *
   * @return the application name
   * @throws DwcjRuntimeException if failed to get the application name
   */
  public static String getApplicationName() {
    try {
      return Environment.getCurrent().getBBjAPI().getWebManager().getApplicationName();
    } catch (BBjException e) {
      throw new DwcjRuntimeException("Failed to get application name.", e);
    }
  }

  /**
   * Get the application protocol.
   *
   * @return The application protocol
   */
  public static String getProtocol() {
    IDwcjBBjBridge helper = Environment.getCurrent().getDwcjHelper();
    Object instance = helper.createInstance("::BBUtils.bbj::BBUtils");

    return (String) helper.invokeMethod(instance, "getWebServerProtocol", null);
  }

  /**
   * Get the application host.
   *
   * @return The application host
   */
  public static String getHost() {
    IDwcjBBjBridge helper = Environment.getCurrent().getDwcjHelper();
    Object instance = helper.createInstance("::BBUtils.bbj::BBUtils");

    return (String) helper.invokeMethod(instance, "getWebServerHost", null);
  }

  /**
   * Get the application port.
   *
   * @return The application port
   */
  public static String getPort() {
    IDwcjBBjBridge helper = Environment.getCurrent().getDwcjHelper();
    Object instance = helper.createInstance("::BBUtils.bbj::BBUtils");

    return (String) helper.invokeMethod(instance, "getWebServerPort", null);
  }

  /**
   * Get the application URL.
   *
   * @return The application URL
   * @throws DwcjRuntimeException if failed to get the application URL
   */
  public static String getUrl() {
    try {
      return Environment.getCurrent().getBBjAPI().getWebManager().getUrl();
    } catch (BBjException e) {
      throw new DwcjRuntimeException("Failed to get application URL.", e);
    }
  }

  /**
   * Set the terminate action for the application when the application terminates normally.
   *
   * @param action The action to be executed on termination
   * @return The application instance
   */
  public App setTerminateAction(AppCloseAction action) {
    setAppAction(action, true);
    return this;
  }

  /**
   * Set the error action for the application when the application terminates with an error.
   *
   * @param action The action to be executed on error
   * @return The application instance
   */
  public App setErrorAction(AppCloseAction action) {
    setAppAction(action, false);
    return this;
  }

  /**
   * Log a String to the browser console (console.out)
   *
   * @param output The message to log
   */
  public static void consoleLog(String output) {
    try {

      Environment.getCurrent().getSysGui().executeScript("console.log(\"" + output + "\")");
    } catch (BBjException e) {
      Environment.logError(e);
    }
  }

  public static void consoleError(String output) {
    try {

      Environment.getCurrent().getSysGui().executeScript("console.error(\"" + output + "\")");
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
    return Environment.getCurrent().getDwcjHelper().msgbox(alert, 0, "");
  }

  /**
   *
   * @param alert The message to show
   * @param options
   * @return
   */
  public static int msgbox(String alert, int options) {
    return Environment.getCurrent().getDwcjHelper().msgbox(alert, options, "");
  }

  /**
   *
   * @param alert The message to show
   * @param options
   * @param title
   * @return
   */
  public static int msgbox(String alert, int options, String title) {
    return Environment.getCurrent().getDwcjHelper().msgbox(alert, options, title);
  }

  /**
   * Show or hide a busy indicator overlay
   *
   * @param busy A boolean value true=show false=hide
   */
  public static void busy(boolean busy) {
    try {
      if (busy) {
        Environment.getCurrent().getBBjAPI().getBuiManager().getBusyIndicator().setText("");
      }
      Environment.getCurrent().getBBjAPI().getBuiManager().getBusyIndicator().setVisible(busy);
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
      Environment.getCurrent().getBBjAPI().getBuiManager().getBusyIndicator().setText(busyText);
      Environment.getCurrent().getBBjAPI().getBuiManager().getBusyIndicator().setVisible(true);
    } catch (BBjException e) {
      // ignore
    }
  }

  private void preRun() {
    Environment.getCurrent().getBBjAPI().setCustomEventCallback("doTerminate", "terminate");
  }

  /**
   * Call this method to terminate your App.
   */
  public void terminate() {
    Environment.getCurrent().getBBjAPI().postPriorityCustomEvent("doTerminate", null);
    cleanup();
    Environment.cleanup();
  }

  /**
   * Override this method to implement custom cleanup e.g. kill all background threads that may
   * still run
   */
  public void cleanup() {}

  /**
   * Gets the CookieStorage.
   *
   * @return the CookieStorage instance
   */
  public static CookieStorage getCookieStorage() {
    return CookieStorage.getCurrent();
  }

  /**
   * Gets the SessionStorage.
   *
   * @return the SessionStorage instance
   */
  public static SessionStorage getSessionStorage() {
    return SessionStorage.getCurrent();
  }

  /**
   * Gets the LocalStorage.
   *
   * @return the LocalStorage instance
   */
  public static LocalStorage getLocalStorage() {
    return LocalStorage.getCurrent();
  }

  /**
   * Override this method to implement your app behavior
   *
   * @throws DwcjAppInitializeException
   */
  public abstract void run() throws DwcjException;

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
    if (prefix.isBlank() || name.isBlank()) {
      throw new IllegalArgumentException("You need a prefix and a name here");
    }

    return new PrivateNamespace(prefix, name, fCreateIfMissing);

  }

  /**
   * Set either the terminate or error app action.
   *
   * @param action The action to set
   * @param isTerminateAction Flag to determine if it's a terminate action or error action
   */
  private void setAppAction(AppCloseAction action, boolean isTerminateAction) {
    try {
      BBjWebManager webManager = getEnvironment().getBBjAPI().getWebManager();

      switch (action.getClass().getSimpleName()) {
        case "RedirectAction":
          URL url = ((RedirectAction) action).getUrl();
          BBjBuiCloseAction redirectAction = webManager.urlAction(url.toString());
          if (isTerminateAction) {
            webManager.setEndAction(redirectAction);
          } else {
            webManager.setErrAction(redirectAction);
          }
          break;
        case "MessageAction":
          String message = ((MessageAction) action).getMessage();
          BBjBuiCloseAction messageAction = webManager.msgAction(message);

          if (isTerminateAction) {
            webManager.setEndAction(messageAction);
          } else {
            webManager.setErrAction(messageAction);
          }
          break;
        case "DefaultAction":
          BBjBuiCloseAction defaultAction = webManager.defaultAction();

          if (isTerminateAction) {
            webManager.setEndAction(defaultAction);
          } else {
            webManager.setErrAction(defaultAction);
          }
          break;
        case "NoneAction":
          BBjBuiCloseAction noneAction = webManager.noneAction();

          if (isTerminateAction) {
            webManager.setEndAction(noneAction);
          } else {
            webManager.setErrAction(noneAction);
          }
          break;
        default:
          throw new IllegalArgumentException(
              "Unsupported " + (isTerminateAction ? "terminate" : "error") + " action.");
      }
    } catch (BBjException e) {
      throw new DwcjRuntimeException(
          "Failed to set app " + (isTerminateAction ? "terminate" : "error") + " action.", e);
    }
  }

  Environment getEnvironment() {
    return Environment.getCurrent();
  }
}
