package com.webforj;

import com.basis.bbj.proxies.BBjBuiCloseAction;
import com.basis.bbj.proxies.BBjWebManager;
import com.basis.bbj.proxies.sysgui.BBjTopLevelWindow;
import com.basis.startup.type.BBjException;
import com.basis.startup.type.BBjVector;
import com.webforj.annotation.AnnotationProcessor;
import com.webforj.annotation.Routify;
import com.webforj.component.optiondialog.OptionDialog;
import com.webforj.component.window.Frame;
import com.webforj.environment.ObjectTable;
import com.webforj.environment.StringTable;
import com.webforj.exceptions.WebforjAppInitializeException;
import com.webforj.exceptions.WebforjException;
import com.webforj.exceptions.WebforjRuntimeException;
import com.webforj.exceptions.WebforjWebManagerException;
import com.webforj.router.NavigationOptions;
import com.webforj.router.RouteRegistry;
import com.webforj.router.Router;
import com.webforj.router.RouterDevUtils;
import com.webforj.router.event.NavigateEvent;
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
   * @throws WebforjException if the app cannot be initialized
   */
  public final void initialize() throws WebforjException {
    if (isInitialized) {
      throw new WebforjAppInitializeException("App is already initialized.");
    }

    try {
      String key = "PARSE_REQUEST_THEME";
      if (StringTable.contains(key) && StringTable.get(key).equals("1")) {
        String theme = Request.getCurrent().getQueryParameter("__theme__");
        if (theme != null) {
          setTheme(theme);
        }
      }

      Page.getCurrent().onUnload(ev -> terminate());

      initializeRouter();
      onWillRun();
      AnnotationProcessor processor = new AnnotationProcessor();
      processor.processAppAnnotations(this);
      createFirstFrame();
      run();
      resolveFirstRoute();
      isInitialized = true;
      onDidRun();
    } catch (Exception e) {
      if (!isCausedByChannelTermination(e)) {
        throw e;
      }
    }
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
   * @throws WebforjWebManagerException if failed to set the theme
   */
  public static void setTheme(String theme) {
    try {
      Environment.getCurrent().getBBjAPI().getWebManager().setTheme(theme);
    } catch (BBjException e) {
      throw new WebforjWebManagerException("Failed to set theme.", e);
    }
  }

  /**
   * Get the application theme.
   *
   * @return The theme
   * @throws WebforjWebManagerException if failed to get the theme
   */
  public static String getTheme() {
    try {
      return Environment.getCurrent().getBBjAPI().getWebManager().getTheme();
    } catch (BBjException e) {
      throw new WebforjWebManagerException("Failed to get theme.", e);
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
      throw new WebforjWebManagerException("Failed to set dark theme.", e);
    }
  }

  /**
   * Get the name of the dark theme.
   *
   * @return The dark theme
   * @throws WebforjWebManagerException if failed to get the dark theme
   */
  public static String getDarkTheme() {
    try {
      return Environment.getCurrent().getBBjAPI().getWebManager().getDarkTheme();
    } catch (BBjException e) {
      throw new WebforjWebManagerException("Failed to get dark theme.", e);
    }
  }

  /**
   * Set the name of the light theme to use for the application. The light theme setting is used
   * when the application theme is set to "system".
   *
   * @param lightTheme The light theme to set
   * @throws WebforjWebManagerException if failed to set the light theme
   */
  public static void setLightTheme(String lightTheme) {
    try {
      Environment.getCurrent().getBBjAPI().getWebManager().setLightTheme(lightTheme);
    } catch (BBjException e) {
      throw new WebforjWebManagerException("Failed to set light theme.", e);
    }
  }

  /**
   * Get the name of the light theme to use for the application.
   *
   * @return The light theme
   * @throws WebforjWebManagerException if failed to get the light theme
   */
  public static String getLightTheme() {
    try {
      return Environment.getCurrent().getBBjAPI().getWebManager().getLightTheme();
    } catch (BBjException e) {
      throw new WebforjWebManagerException("Failed to get light theme.", e);
    }
  }

  /**
   * Get the registered DWC application name.
   *
   * @return the application name
   * @throws WebforjWebManagerException if failed to get the application name
   */
  public static String getApplicationName() {
    try {
      return Environment.getCurrent().getBBjAPI().getWebManager().getApplicationName();
    } catch (BBjException e) {
      throw new WebforjWebManagerException("Failed to get application name.", e);
    }
  }

  /**
   * Get the application port.
   *
   * @return The application port
   * @deprecated Since 24.02, for removal in 25.00. Use {@link Request#getPort()} instead
   */
  @Deprecated(since = "24.02", forRemoval = true)
  public static String getPort() {
    return Request.getCurrent().getPort();
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
    return Request.getCurrent().getUrl();
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
      throw new WebforjWebManagerException(e);
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
    return Environment.getCurrent().getBridge().msgbox(alert, 0, "");
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
    return Environment.getCurrent().getBridge().msgbox(alert, options, "");
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
    return Environment.getCurrent().getBridge().msgbox(alert, options, title);
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
   * Terminate the application.
   */
  public final void terminate() {
    onWillTerminate();

    // dispose the page
    Page page = Page.getCurrent();
    if (page != null) {
      page.destroy();
    }

    // dispose the router
    Router router = Router.getCurrent();
    if (router != null) {
      router.removeAllListeners();
    }

    // dispose the frames
    List<Frame> frames = getFrames();
    for (Frame frame : frames) {
      frame.destroy();
    }

    Environment.getCurrent().getBBjAPI().postPriorityCustomEvent("webforjTerminateSignal", null);

    onDidTerminate();
  }

  /**
   * Override this method to implement custom cleanup e.g. kill all background threads that may
   * still run
   *
   * @deprecated since 24.10, for removal in 25.00. Use {@link #onWillTerminate()} and
   */
  @Deprecated(since = "24.10", forRemoval = true)
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
   * The main entry point to implement custom application logic.
   *
   * <p>
   * This method is intended to be overridden by any class extending {@code App}. The framework
   * calls this method after the application has been fully initialized. The method is meant to
   * contain the core functionality of the application.
   * </p>
   *
   * <p>
   * Developers should override this method to define the behavior and execution flow of their
   * application. It is called once during the application lifecycle, and should not be invoked
   * directly from outside the class.
   * </p>
   *
   * <p>
   * If the application uses routing (determined by the presence of the {@link Routify} annotation),
   * this method is called after the initial route resolution and frame creation. Otherwise, it is
   * the place where non-routable application logic can be executed.
   * </p>
   *
   * @throws WebforjException if any error occurs during the execution of the application logic.
   */
  public void run() throws WebforjException {
    // no-op
  }

  /**
   * A hook method that is called after the app is run.
   *
   * <p>
   * This method is called after the app has been completely initialized. It is called only once
   * during the app's lifecycle before the {@code run()} method is called. Use this method to
   * perform any additional initialization tasks.
   * </p>
   */
  protected void onWillRun() {
    // no-op
  }

  /**
   * A hook method that is called after the app is run.
   *
   * <p>
   * This method is called after the app has been completely initialized. It is called only once
   * during the app's lifecycle after the {@code run()} method is called. Use this method to perform
   * any additional initialization tasks.
   * </p>
   */
  protected void onDidRun() {
    // no-op
  }

  /**
   * A hook method that is called before the app is terminated.
   *
   * <p>
   * This method is called before the app is terminated. Use this method to perform any cleanup
   * tasks.
   * </p>
   */
  protected void onWillTerminate() {
    // no-op
  }

  /**
   * A hook method that is called after the app is terminated.
   *
   * <p>
   * This method is called after the app is terminated. Use this method to perform any cleanup
   * tasks.
   * </p>
   */
  protected void onDidTerminate() {
    // no-op
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

  /**
   * Initializes the router for the application if routing is enabled.
   *
   * <p>
   * This method is responsible for setting up the routing system in the application. It checks if
   * the app is routable by inspecting the {@link Routify} annotation. If the annotation is present,
   * it retrieves the list of packages to be scanned for route definitions and initializes the
   * {@link Router} and {@link RouteRegistry}.
   * </p>
   *
   * <p>
   * The router is initialized with the root directory of the web application and the registry of
   * routes, which is built by scanning the specified packages for route classes and annotations.
   * The router instance is then stored in the {@link ObjectTable} for access during runtime through
   * {@link Router#getCurrent()}.
   * </p>
   *
   * <p>
   * If no packages are specified in the {@link Routify} annotation, the framework defaults to using
   * the package of the application class.
   * </p>
   *
   * <p>
   * If the app is not routable, this method will not initialize any routing components.
   * </p>
   *
   * @see Router
   * @see RouteRegistry
   * @see Routify
   */
  private void initializeRouter() {
    if (!isRoutable()) {
      return;
    }

    String[] packages = getClass().getAnnotation(Routify.class).packages();
    if (packages.length == 0) {
      // default package
      packages = new String[] {getClass().getPackageName()};
    }

    String root = "webapp/" + getApplicationName();
    boolean isBbjService = Environment.isRunningWithBBjServices();
    if (!isBbjService) {
      root = Environment.getContextPath();
      root = root.isBlank() ? "/" : root;
    }

    RouteRegistry registry = RouteRegistry.ofPackage(packages);
    Router router = new Router(root, registry);
    ObjectTable.put(Router.class.getName(), router);

    if (getClass().getAnnotation(Routify.class).manageFramesVisibility()) {
      router.onNavigate(this::handleFramesVisibility);
    }

    boolean debug = getClass().getAnnotation(Routify.class).debug();
    router.setDebug(debug);

    if (debug) {
      RouterDevUtils.logRouteEntires(router.getRegistry().getAvailableRouteEntires());
    }
  }

  /**
   * Creates the first application frame if routing is enabled and the frame initialization flag is
   * set.
   *
   * <p>
   * This method is responsible for creating the first frame in the application if the app is
   * routable and the {@code initializeFrame} attribute in the {@link Routify} annotation is set to
   * {@code true}. This frame serves as the main window or interface element for the application.
   * </p>
   *
   * <p>
   * If the application is not routable or if frame initialization is disabled, this method will not
   * create a frame. The method ensures that frame creation only occurs when explicitly requested
   * through the {@link Routify} annotation.
   * </p>
   *
   * @throws WebforjAppInitializeException if the frame cannot be created
   * @see Frame
   * @see Routify
   */
  private void createFirstFrame() throws WebforjAppInitializeException {
    if (!isRoutable()) {
      return;
    }

    boolean initFrame = getClass().getAnnotation(Routify.class).initializeFrame();

    if (initFrame) {
      Frame defaultFrame = new Frame();
      defaultFrame.setName(getClass().getAnnotation(Routify.class).defaultFrameName());
    }
  }

  /**
   * Resolves the first route for the application when routing is enabled.
   *
   * <p>
   * This method is responsible for determining the initial route when the application is started.
   * It interacts with the {@link Router} instance to obtain the current location from the browser's
   * history and navigates to it using the provided {@link NavigationOptions}. This ensures that the
   * app starts at the correct route based on the URL.
   * </p>
   *
   * <p>
   * If the application is not routable (i.e., it is not annotated with {@link Routify}), this
   * method will not take any action.
   * </p>
   *
   * <p>
   * The navigation is performed using the {@code REPLACE} navigation type, which updates the
   * current route without adding a new entry to the browser history.
   * </p>
   *
   * @see Router#navigate(String, NavigationOptions)
   * @see NavigationOptions
   */
  private void resolveFirstRoute() {
    if (!isRoutable()) {
      return;
    }

    Router router = Router.getCurrent();
    if (router != null) {
      router.getHistory().getLocation().ifPresent(location -> {
        NavigationOptions options = new NavigationOptions();
        options.setUpdateHistory(false);
        Router.getCurrent().navigate(location, options);
      });
    }
  }

  /**
   * Handles frame visibility based on the matched route during navigation.
   *
   * <p>
   * This method is called when the application is navigated to a new route. It is responsible for
   * toggling the visibility of frames based on the matched route. When the app is navigated to a
   * new route, the matched frame is shown, and all other frames are hidden.
   * </p>
   *
   * <p>
   * This method is registered as a navigation event listener in the {@link Router} instance during
   * the initialization phase. It is called after the router has successfully navigated to a new
   * route and attached its component to the DOM and updated the history.
   * </p>
   *
   * @param event The navigation event object
   */
  private void handleFramesVisibility(NavigateEvent event) {
    event.getContext().getActiveFrame().ifPresent(activeFrame -> {
      List<Frame> frames = App.getFrames();
      frames.forEach(f -> {
        if (!f.equals(activeFrame)) {
          f.setVisible(false);
        }
      });

      activeFrame.setVisible(true);
    });
  }

  /**
   * Checks if the application is routable by inspecting the presence of the {@link Routify}
   * annotation.
   *
   * @return {@code true} if the app is routable, {@code false} otherwise
   */
  private boolean isRoutable() {
    return getClass().isAnnotationPresent(Routify.class);
  }

  /**
   * Checks if the given exception is caused by a channel termination.
   *
   * @param e The exception to check
   * @return {@code true} if the exception is caused by a channel termination, {@code false}
   *         otherwise
   */
  private static boolean isCausedByChannelTermination(Throwable e) {
    while (e != null) {
      if (e instanceof BBjException && "Channel has been terminated".equals(e.getMessage())) {
        return true;
      }

      e = e.getCause();
    }

    return false;
  }

  Environment getEnvironment() {
    return Environment.getCurrent();
  }
}
