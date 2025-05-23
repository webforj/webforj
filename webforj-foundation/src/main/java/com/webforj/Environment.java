package com.webforj;

import com.basis.bbj.proxies.BBjAPI;
import com.basis.bbj.proxies.BBjSysGui;
import com.basis.startup.type.BBjException;
import com.basis.util.common.Util;
import com.typesafe.config.Config;
import com.typesafe.config.ConfigFactory;
import com.webforj.bridge.WebforjBBjBridge;
import com.webforj.environment.ObjectTable;
import com.webforj.error.ErrorHandler;
import com.webforj.error.GlobalErrorHandler;
import java.lang.reflect.InvocationTargetException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Iterator;
import java.util.ServiceLoader;
import java.util.function.Consumer;

/**
 * The Environment class represents the runtime environment for the webforj application. It provides
 * methods to initialize, configure, and manage the environment, including handling errors, managing
 * debug mode, and accessing configuration settings.
 *
 * <p>
 * This class is designed to be used in a multi-threaded context, with each thread having its own
 * instance of the Environment.
 * </p>
 *
 * @author Stephan Wald
 * @author Hyyan Abo Fakher
 * @since 0.001
 */
public final class Environment {

  private static final String RESOURCE_PREFIX = "!!";
  private static final HashMap<Object, Environment> instanceMap = new HashMap<>();
  private final BBjAPI api;
  private final BBjSysGui sysgui;
  private final WebforjBBjBridge bridge;
  private boolean debug = false;

  /**
   * Creates a new environment.
   *
   * @param api the BBjAPI instance.
   * @param bridge the WebforjBBjBridge instance.
   * @param debug {@code true} if debug mode is enabled, {@code false} otherwise.
   * @throws BBjException if an error occurs while creating the environment.
   */
  private Environment(BBjAPI api, WebforjBBjBridge bridge, boolean debug) throws BBjException {
    this.api = api;
    this.sysgui = api.openSysGui("X0");
    this.bridge = bridge;
    this.debug = debug;
  }

  /**
   * Initializes the environment for the current thread.
   *
   * @param api the BBjAPI instance.
   * @param helper the WebforjBBjBridge instance.
   * @param debug {@code 1} if debug mode is enabled, {@code 0} otherwise.
   * @throws BBjException if an error occurs while initializing the environment.
   */
  public static void init(BBjAPI api, WebforjBBjBridge helper, int debug) throws BBjException {
    Environment env = new Environment(api, helper, debug > 0);
    Environment.instanceMap.put(Thread.currentThread().getId(), env);
  }

  /**
   * Cleans up the environment for the current thread.
   */
  public static void cleanup() {
    Environment.instanceMap.remove(Thread.currentThread().getId());
  }

  /**
   * Returns the environment for the current thread.
   *
   * @return the environment for the current thread.
   */
  public static Environment getCurrent() {
    return Environment.instanceMap.get(Thread.currentThread().getId());
  }

  /**
   * Check if the environment is present.
   *
   * @return true if the environment is present
   */
  public static boolean isPresent() {
    return getCurrent() != null;
  }

  /**
   * Executes the given consumer with the environment for the current thread when it is available.
   *
   * @param consumer the consumer to execute
   * @since 24.22
   */
  public static void ifPresent(Consumer<Environment> consumer) {
    if (isPresent()) {
      consumer.accept(getCurrent());
    }
  }

  /**
   * Enables or disables debug mode for the current environment.
   *
   * @param debug true to enable debug mode, false to disable it.
   * @since 24.20
   */
  void setDebug(boolean debug) {
    this.debug = debug;
  }

  /**
   * Checks if debug mode is enabled for the current environment.
   *
   * @return true if debug mode is enabled, false otherwise.
   * @since 24.10
   */
  public boolean isDebug() {
    return this.debug;
  }

  /**
   * Checks if webforj is running inside BBj Services.
   *
   * @return true if webforj is running inside BBj Services, false otherwise.
   * @since 24.20
   */
  public static boolean isRunningWithBBjServices() {
    String isNoBbjService = Util.getProperty("com.basis.noBBjServices", "");
    return !isNoBbjService.equalsIgnoreCase("true");
  }

  /**
   * Returns the context path for the current environment.
   *
   * <p>
   * The context path is the base path for the web application. It is used to resolve relative URLs
   * and to determine the root path for the application.
   * </p>
   *
   * @return the context path for the current environment.
   * @since 25.00
   */
  public static String getContextPath() {
    return Util.getProperty("webforj.context", "/");
  }

  /**
   * Suspends execution of the current task.
   *
   * <p>
   * wait may not be precisely accurate because it can be altered by other factors that govern timed
   * pauses in a system. wait statements are not affected by processor speed.
   * </p>
   *
   * @param seconds Number of seconds to suspend the current task. It must be in the range of 0 to
   *        255. (Some systems may allow waits longer than 255 seconds.)
   */
  public void sleep(int seconds) {
    getBridge().sleep(seconds);
  }

  /**
   * Returns the configuration for the current environment.
   *
   * <p>
   * The configuration is loaded from the file specified by the {@code webforj.conf}.
   * </p>
   *
   * @return the configuration for the current environment.
   *
   * @since 24.20
   */
  public Config getConfig() {
    String lookupKey = "webforj.configuration";
    if (ObjectTable.contains(lookupKey)) {
      return (Config) ObjectTable.get(lookupKey);
    }

    String pathProp = Util.getProperty("webforj.conf", "!!webforj.conf");
    Config config;

    if (pathProp.isEmpty()) {
      config = getDefaultConfig();
    } else if (pathProp.startsWith(RESOURCE_PREFIX)) {
      final String resourcePath =
          pathProp.isEmpty() ? "webforj.conf" : pathProp.substring(RESOURCE_PREFIX.length());
      final Config resourceConfig = ConfigFactory.parseResourcesAnySyntax(
          Environment.getCurrent().getClass().getClassLoader(), resourcePath);
      if (null == resourceConfig) {
        config = getDefaultConfig();
      } else {
        config = resourceConfig.withFallback(getDefaultConfig());
      }
    } else {
      final Path configPath = Paths.get(pathProp);
      config = ConfigFactory.parseFile(configPath.toFile()).withFallback(getDefaultConfig());
    }

    return config;
  }

  /**
   * Returns the default configuration for the current environment.
   *
   * @return the default configuration for the current environment.
   * @since 24.20
   */
  Config getDefaultConfig() {
    return ConfigFactory.parseResourcesAnySyntax(
        Environment.getCurrent().getClass().getClassLoader(), "webforj-default.conf");
  }

  /**
   * Handles an error that occurred during the execution of the application.
   *
   * <p>
   * This method will attempt to find the appropriate error handler for the given exception and
   * invoke the {@link ErrorHandler#onError(Throwable, boolean)} method. BBj will use this method to
   * forward errors caught in the BBj environment to the webforJ error handling system.
   * </p>
   *
   * @param ex the exception that occurred.
   * @param debug {@code 1} if debug mode is enabled, {@code 0} otherwise.
   *
   * @since 24.12
   */
  public static void handleError(Throwable ex, int debug) {
    handleError(ex, debug, ServiceLoader.load(ErrorHandler.class));
  }

  /**
   * Handles an error that occurred during the execution of the application.
   *
   * <p>
   * This method will attempt to find the appropriate error handler for the given exception and
   * invoke the {@link ErrorHandler#onError(Throwable, boolean)} method. BBj will use this method to
   * forward errors caught in the BBj environment to the webforJ error handling system.
   * </p>
   *
   * @param ex the exception that occurred.
   * @param debug {@code 1} if debug mode is enabled, {@code 0} otherwise.
   * @param serviceLoader the service loader to use to find error handlers.
   *
   * @since 24.12
   */
  static void handleError(Throwable ex, int debug, ServiceLoader<ErrorHandler> serviceLoader) {
    ErrorHandler handler = findHandlerForError(ex, serviceLoader);
    handler.onError(ex, debug > 0);
  }

  public BBjAPI getBBjAPI() {
    return this.api;
  }

  public BBjSysGui getSysGui() {
    return this.sysgui;
  }

  /**
   * Returns the WebforjBBjBridge instance.
   *
   * @return the WebforjBBjBridge instance.
   */
  public WebforjBBjBridge getBridge() {
    return bridge;
  }

  /**
   * Finds the appropriate error handler for the given exception.
   *
   * @param exception the exception for which to find an error handler.
   * @param serviceLoader the service loader to use to find error handlers.
   *
   * @return the appropriate error handler.
   *
   * @since 24.12
   */
  private static ErrorHandler findHandlerForError(Throwable exception,
      ServiceLoader<ErrorHandler> serviceLoader) {
    String throwableHandlerName = exception.getClass().getSimpleName() + "ErrorHandler";

    ErrorHandler customSpecificHandler = null;
    ErrorHandler customGlobalHandler = null;

    Iterator<ErrorHandler> iterator = serviceLoader.iterator();
    while (iterator.hasNext()) {
      ErrorHandler handler = iterator.next();
      String handlerName = handler.getClass().getSimpleName();

      // Check for specific handler for the exception
      if (handlerName.equals(throwableHandlerName)) {
        customSpecificHandler = handler;
      }

      // Check for global handler
      if (handlerName.equals("WebforjGlobalErrorHandler")) {
        customGlobalHandler = handler;
      }
    }

    if (customSpecificHandler != null) {
      return customSpecificHandler;
    } else {
      try {
        Class<?> defaultHandler = Class.forName("com.webforj.error." + throwableHandlerName);
        return (ErrorHandler) defaultHandler.getDeclaredConstructor().newInstance();
      } catch (ClassNotFoundException | InstantiationException | IllegalAccessException
          | IllegalArgumentException | InvocationTargetException | NoSuchMethodException
          | SecurityException e) {
        // do nothing and continue
      }
    }

    if (customGlobalHandler != null) {
      return customGlobalHandler;
    }

    return getGlobalErrorHandler();
  }

  static ErrorHandler getGlobalErrorHandler() {
    return new GlobalErrorHandler();
  }
}
