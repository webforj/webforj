package com.webforj;

import com.basis.bbj.proxies.BBjBuiCloseAction;
import com.basis.bbj.proxies.BBjWebManager;
import com.basis.bbj.proxies.sysgui.BBjTopLevelWindow;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import com.basis.startup.type.BBjVector;
import com.webforj.annotation.AnnotationProcessor;
import com.webforj.bridge.ComponentAccessor;
import com.webforj.bridge.WebforjBBjBridge;
import com.webforj.component.optiondialog.OptionDialog;
import com.webforj.component.window.Frame;
import com.webforj.environment.StringTable;
import com.webforj.environment.namespace.GlobalNamespace;
import com.webforj.environment.namespace.GroupNamespace;
import com.webforj.environment.namespace.Namespace;
import com.webforj.environment.namespace.PrivateNamespace;
import com.webforj.exceptions.WebforjAppInitializeException;
import com.webforj.exceptions.WebforjException;
import com.webforj.exceptions.WebforjRuntimeException;
import com.webforj.webstorage.CookieStorage;
import com.webforj.webstorage.LocalStorage;
import com.webforj.webstorage.SessionStorage;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

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
   * @throws WebforjAppInitializeException if failed to initialize the app
   */
  public final void initialize() throws WebforjAppInitializeException {
    if (isInitialized) {
      throw new WebforjAppInitializeException("App is already initialized.");
    }

    String key = "PARSE_REQUEST_THEME";
    if (StringTable.contains(key) && StringTable.get(key).equals("1")) {
      String theme = Request.getCurrent().getQueryParameter("__theme__");
      if (theme != null) {
        setTheme(theme);
      }
    }

    preRun();
    try {
      AnnotationProcessor processor = new AnnotationProcessor();
      processor.processAppAnnotations(this, AnnotationProcessor.RunningPhase.PRE_RUN);
      run();
      processor.processAppAnnotations(this, AnnotationProcessor.RunningPhase.POST_RUN);
      isInitialized = true;
    } catch (WebforjException e) {
      Environment.logError(e);
    }
  }

  /**
   * Get the current page instance.
   *
   * @return the current page instance
   * @deprecated since 24.10, for removal in 25.00. Use {@link Page#getCurrent()} instead
   */
  @Deprecated(since = "24.10", forRemoval = true)
  public static Page getPage() {
    return Page.getCurrent();
  }

  /**
   * Get the current request instance.
   *
   * @return the current request instance
   * @deprecated since 24.10, for removal in 25.00. Use {@link Request#getCurrent()} instead
   */
  @Deprecated(since = "24.10", forRemoval = true)
  public static Request getRequest() {
    return Request.getCurrent();
  }

  /**
   * Sets the locale used by the application.
   *
   * @param locale The locale to use
   */
  public static void setLocale(Locale locale) {
    StringTable.put("!LOCALE", locale.toString());
  }

  /**
   * Gets the locale used by the application.
   *
   * <p>
   * This method will attempt to determine the application's locale using its configuration
   * settings. If the locale isn't specified or if there's an error in parsing the locale, it
   * defaults to the JVM's locale.
   * </p>
   *
   * @return the user locale
   */
  public static Locale getLocale() {
    try {
      return new Locale(StringTable.get("!LOCALE"));
    } catch (Exception e) {
      return Locale.getDefault();
    }
  }

  /**
   * Set the application theme.
   *
   * @param theme The theme to set
   * @throws WebforjRuntimeException if failed to set the theme
   */
  public static void setTheme(String theme) {
    try {
      Environment.getCurrent().getBBjAPI().getWebManager().setTheme(theme);
    } catch (BBjException e) {
      throw new WebforjRuntimeException("Failed to set theme.", e);
    }
  }

  /**
   * Get the application theme.
   *
   * @return The theme
   * @throws WebforjRuntimeException if failed to get the theme
   */
  public static String getTheme() {
    try {
      return Environment.getCurrent().getBBjAPI().getWebManager().getTheme();
    } catch (BBjException e) {
      throw new WebforjRuntimeException("Failed to get theme.", e);
    }
  }

  /**
   * Set the name of the dark theme to use for the application. The dark theme setting is used when
   * the application theme is set to "system".
   *
   * @param darkTheme The dark theme to set
   * @throws WebforjRuntimeException if failed to set the dark theme
   */
  public static void setDarkTheme(String darkTheme) {
    try {
      Environment.getCurrent().getBBjAPI().getWebManager().setDarkTheme(darkTheme);
    } catch (BBjException e) {
      throw new WebforjRuntimeException("Failed to set dark theme.", e);
    }
  }

  /**
   * Get the name of the dark theme.
   *
   * @return The dark theme
   * @throws WebforjRuntimeException if failed to get the dark theme
   */
  public static String getDarkTheme() {
    try {
      return Environment.getCurrent().getBBjAPI().getWebManager().getDarkTheme();
    } catch (BBjException e) {
      throw new WebforjRuntimeException("Failed to get dark theme.", e);
    }
  }

  /**
   * Set the name of the light theme to use for the application. The light theme setting is used
   * when the application theme is set to "system".
   *
   * @param lightTheme The light theme to set
   * @throws WebforjRuntimeException if failed to set the light theme
   */

  public static void setLightTheme(String lightTheme) {
    try {
      Environment.getCurrent().getBBjAPI().getWebManager().setLightTheme(lightTheme);
    } catch (BBjException e) {
      throw new WebforjRuntimeException("Failed to set light theme.", e);
    }
  }

  /**
   * Get the name of the light theme to use for the application.
   *
   * @return The light theme
   * @throws WebforjRuntimeException if failed to get the light theme
   */

  public static String getLightTheme() {
    try {
      return Environment.getCurrent().getBBjAPI().getWebManager().getLightTheme();
    } catch (BBjException e) {
      throw new WebforjRuntimeException("Failed to get light theme.", e);
    }
  }

  /**
   * Get the registered DWC application name.
   *
   * @return the application name
   * @throws WebforjRuntimeException if failed to get the application name
   */
  public static String getApplicationName() {
    try {
      return Environment.getCurrent().getBBjAPI().getWebManager().getApplicationName();
    } catch (BBjException e) {
      throw new WebforjRuntimeException("Failed to get application name.", e);
    }
  }

  /**
   * Get the application protocol.
   *
   * @return The application protocol
   * @deprecated since 24.02, for removal in 25.00. Use {@link Request#getProtocol()} instead
   */
  @Deprecated(since = "24.02", forRemoval = true)
  public static String getProtocol() {
    WebforjBBjBridge helper = Environment.getCurrent().getWebforjHelper();
    Object instance = helper.createInstance("::BBUtils.bbj::BBUtils");

    return (String) helper.invokeMethod(instance, "getWebServerProtocol", null);
  }

  /**
   * Get the application host.
   *
   * @return The application host
   * @deprecated Since 24.02, for removal in 25.00. Use {@link Request#getHost()} instead
   */
  @Deprecated(since = "24.02", forRemoval = true)
  public static String getHost() {
    WebforjBBjBridge helper = Environment.getCurrent().getWebforjHelper();
    Object instance = helper.createInstance("::BBUtils.bbj::BBUtils");

    return (String) helper.invokeMethod(instance, "getWebServerHost", null);
  }

  /**
   * Get the application port.
   *
   * @return The application port
   * @deprecated Since 24.02, for removal in 25.00. Use {@link Request#getPort()} instead
   */
  @Deprecated(since = "24.02", forRemoval = true)
  public static String getPort() {
    WebforjBBjBridge helper = Environment.getCurrent().getWebforjHelper();
    Object instance = helper.createInstance("::BBUtils.bbj::BBUtils");

    return (String) helper.invokeMethod(instance, "getWebServerPort", null);
  }

  /**
   * Get the application URL.
   *
   * @return The application URL
   * @throws WebforjRuntimeException if failed to get the application URL
   *
   * @deprecated Since 24.02, for removal in 25.00. Use {@link Request#getUrl()} instead.
   */
  @Deprecated(since = "24.02", forRemoval = true)
  public static String getUrl() {
    try {
      return Environment.getCurrent().getBBjAPI().getWebManager().getUrl();
    } catch (BBjException e) {
      throw new WebforjRuntimeException("Failed to get application URL.", e);
    }
  }

  /**
   * Get the console instance.
   *
   * @return the console instance
   */
  public static BrowserConsole console() {
    return new BrowserConsole();
  }

  /**
   * Gets the busy indicator instance.
   *
   * @return the busy indicator instance
   * @since 24.10
   */
  public static BusyIndicator getBusyIndicator() {
    try {
      return new BusyIndicator(
          Environment.getCurrent().getBBjAPI().getWebManager().getBusyIndicator());
    } catch (BBjException e) {
      throw new WebforjRuntimeException(e);
    }
  }

  /**
   * Shows or hides the application busy indicator.
   *
   * @param busy true to show the busy indicator, false to hide it
   * @return the busy indicator instance
   *
   * @since 24.10
   */
  public static BusyIndicator busy(boolean busy) {
    return getBusyIndicator().setVisible(busy);
  }

  /**
   * Shows the application busy indicator with the specified text message.
   *
   * @param text The text message to show
   * @param allowHtml true to allow HTML in the text message, false otherwise
   * @return the busy indicator instance
   *
   * @since 24.10
   */
  public static BusyIndicator busy(String text, boolean allowHtml) {
    BusyIndicator indicator = getBusyIndicator();
    if (allowHtml) {
      indicator.setHtml(text);
    } else {
      indicator.setText(text);
    }

    indicator.setVisible(true);
    return indicator;
  }

  /**
   * Shows the application busy indicator with the specified text message.
   *
   * @param text The text message to show
   * @return the busy indicator instance
   *
   * @since 24.10
   */
  public static BusyIndicator busy(String text) {
    return busy(text, false);
  }

  /**
   * Log a String to the browser console (console.log)
   *
   * @param output The message to log
   * @deprecated since 24.10, for removal in 25.00. Use {@link BrowserConsole#log(String)} instead
   */
  @Deprecated(since = "24.10", forRemoval = true)
  public static void consoleLog(String output) {
    try {

      Environment.getCurrent().getSysGui().executeScript("console.log(\"" + output + "\")");
    } catch (BBjException e) {
      Environment.logError(e);
    }
  }

  /**
   * Log a String to the browser console (console.error)
   *
   * @param output The message to log
   * @deprecated since 24.10, for removal in 25.00. Use {@link BrowserConsole#error(String)} instead
   */
  @Deprecated(since = "24.10", forRemoval = true)
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
   *
   * @deprecated since 24.02, for removal in 25.00
   */
  @Deprecated(since = "24.02", forRemoval = true)
  public static int msgbox(String alert) {
    return Environment.getCurrent().getWebforjHelper().msgbox(alert, 0, "");
  }

  /**
   *
   * @param alert The message to show
   * @param options
   * @return
   *
   * @deprecated since 24.02, for removal in 25.00. Use
   *             {@link OptionDialog#showMessageDialog(Object)}
   */
  @Deprecated(since = "24.02", forRemoval = true)
  public static int msgbox(String alert, int options) {
    return Environment.getCurrent().getWebforjHelper().msgbox(alert, options, "");
  }

  /**
   *
   * @param alert The message to show
   * @param options
   * @param title
   * @return
   *
   * @deprecated since 24.02, for removal in 25.00. Use
   *             {@link OptionDialog#showMessageDialog(Object)}
   */
  @Deprecated(since = "24.02", forRemoval = true)
  public static int msgbox(String alert, int options, String title) {
    return Environment.getCurrent().getWebforjHelper().msgbox(alert, options, title);
  }

  /**
   * Get the list of all created frames in the application.
   *
   * @return the list of all frames in the application
   */
  public static List<Frame> getFrames() {
    BBjVector windows = Environment.getCurrent().getSysGui().getWindows();
    List<Frame> frames = new ArrayList<>();

    try {
      for (int i = 0; i < windows.size(); i++) {
        Object window = windows.get(i);
        if (window instanceof BBjTopLevelWindow) {
          BBjTopLevelWindow topLevelWindow = (BBjTopLevelWindow) window;
          Object userData = topLevelWindow.getUserData();
          if (userData instanceof Frame frame) {
            frames.add(frame);
          }
        }
      }
    } catch (BBjException e) {
      // pass
    }

    return frames;
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
   * @deprecated since 24.10, for removal in 25.00. Use {@link CookieStorage#getCurrent()} instead
   */
  @Deprecated(since = "24.10", forRemoval = true)
  public static CookieStorage getCookieStorage() {
    return CookieStorage.getCurrent();
  }

  /**
   * Gets the SessionStorage.
   *
   * @return the SessionStorage instance
   * @deprecated since 24.10, for removal in 25.00. Use {@link SessionStorage#getCurrent()} instead
   */
  @Deprecated(since = "24.10", forRemoval = true)
  public static SessionStorage getSessionStorage() {
    return SessionStorage.getCurrent();
  }

  /**
   * Gets the LocalStorage.
   *
   * @return the LocalStorage instance
   * @deprecated since 24.10, for removal in 25.00. Use {@link LocalStorage#getCurrent()} instead
   */
  @Deprecated(since = "24.10", forRemoval = true)
  public static LocalStorage getLocalStorage() {
    return LocalStorage.getCurrent();
  }

  /**
   * Override this method to implement your app behavior.
   *
   * @throws WebforjException if an error occurs
   */
  public abstract void run() throws WebforjException;

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

  private void preRun() {
    Environment.getCurrent().getBBjAPI().setCustomEventCallback("doTerminate", "terminate");
  }

  /**
   * Set either the terminate or error app action.
   *
   * @param action The action to set
   * @param isTerminateAction Flag to determine if it's a terminate action or error action
   */
  @SuppressWarnings("squid:S3776")
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
      throw new WebforjRuntimeException(
          "Failed to set app " + (isTerminateAction ? "terminate" : "error") + " action.", e);
    }
  }

  Environment getEnvironment() {
    return Environment.getCurrent();
  }
}
