package com.webforj;

import com.basis.bbj.proxies.BBjAPI;
import com.basis.bbj.proxies.BBjSysGui;
import com.basis.startup.type.BBjException;
import com.basis.util.common.Util;
import com.typesafe.config.Config;
import com.typesafe.config.ConfigFactory;
import com.webforj.annotation.Experimental;
import com.webforj.bridge.WebforjBBjBridge;
import com.webforj.environment.ObjectTable;
import com.webforj.error.ErrorHandler;
import com.webforj.error.GlobalErrorHandler;
import java.lang.System.Logger;
import java.lang.System.Logger.Level;
import java.lang.reflect.InvocationTargetException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Queue;
import java.util.ServiceLoader;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Consumer;
import java.util.function.Supplier;

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
  private static final Logger logger = System.getLogger(Environment.class.getName());
  private static final String RESOURCE_PREFIX = "!!";
  private static final HashMap<Object, Environment> instanceMap = new HashMap<>();
  private static final AtomicInteger accessRequestCounter = new AtomicInteger(0);
  private static final InheritableThreadLocal<Environment> inheritableEnvironment =
      new InheritableThreadLocal<>();
  private final BBjAPI api;
  private final BBjSysGui sysgui;
  private final WebforjBBjBridge bridge;
  private final Queue<EnvironmentAccessRequest> accessQueue = new ConcurrentLinkedQueue<>();
  private final HashMap<Integer, PendingResult<Object>> pendingResults = new HashMap<>();
  private Interval laterQueueTimer;
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
   *
   * @throws BBjException if an error occurs while initializing the environment.
   */
  public static void init(BBjAPI api, WebforjBBjBridge helper, int debug) throws BBjException {
    Environment env = new Environment(api, helper, debug > 0);
    Long currentThreadId = Thread.currentThread().getId();
    Environment.instanceMap.put(currentThreadId, env);

    // Store in inheritable thread local so child threads can access it
    inheritableEnvironment.set(env);

    // Create and start the runLater queue timer
    env.createRunLaterQueueTimer();
  }

  /**
   * Cleans up the environment for the current thread.
   */
  public static void cleanup() {
    Long currentThreadId = Thread.currentThread().getId();
    Environment env = Environment.instanceMap.remove(currentThreadId);

    if (env != null) {
      // Stop the run later queue timer
      var runLaterQueue = env.getLaterQueueTimer();
      if (runLaterQueue != null && runLaterQueue.isRunning()) {
        runLaterQueue.stop();
      }

      // Clear from inheritable thread local
      inheritableEnvironment.remove();
    }
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
   * Executes the given task in an Environment's thread context. This method allows safe access to
   * the Environment from background threads.
   *
   * <p>
   * ⚠️ WARNING: This method is experimental since 25.02 and may change in future releases.
   * </p>
   *
   * <p>
   * If called from an Environment thread, the task is executed immediately. If called from a
   * background thread that was started from an Environment thread, the task is queued and executed
   * asynchronously in that Environment's thread.
   * </p>
   *
   * <p>
   * Example usage:
   * </p>
   *
   * <pre>{@code
   * // From a background thread
   * executorService.submit(() -> {
   *   Environment.runLater(() -> {
   *     // This code runs in the Environment thread
   *     button.setText("Updated from background");
   *     textField.setEnabled(true);
   *   });
   * });
   * }</pre>
   *
   * @param task the task to execute
   *
   * @return a PendingResult that completes when the task is executed
   * @throws IllegalStateException if called from a thread with no associated Environment
   *
   * @since 25.02
   */
  @Experimental(since = "25.02")
  public static PendingResult<Void> runLater(Runnable task) {
    return runLater(() -> {
      task.run();
      return null;
    });
  }

  /**
   * Executes the given supplier in an Environment's thread context and returns its result. This
   * method allows safe access to the Environment from background threads.
   *
   * <p>
   * ⚠️ WARNING: This method is experimental since 25.02 and may change in future releases.
   * </p>
   *
   * <p>
   * If called from an Environment thread, the supplier is executed immediately. If called from a
   * background thread that was started from an Environment thread, the supplier is queued and
   * executed asynchronously in that Environment's thread.
   * </p>
   *
   * @param <T> the type of the result
   * @param supplier the supplier to execute
   *
   * @return a PendingResult that completes with the supplier's result
   * @throws IllegalStateException if called from a thread with no associated Environment
   *
   * @since 25.02
   */
  @Experimental(since = "25.02")
  public static <T> PendingResult<T> runLater(Supplier<T> supplier) {
    // Find the Environment for the current thread
    Environment env = findEnvironmentForThread();

    if (env == null) {
      throw new IllegalStateException("No Environment associated with current thread. "
          + "Environment.runLater() can only be called from threads started within an "
          + "Environment context.");
    }

    // Check if we're already in the Environment's thread
    Environment current = getCurrent();
    if (current == env) {
      logger.log(Level.DEBUG, "runLater Called from UI thread, executing immediately");
      try {
        T result = supplier.get();
        return PendingResult.completedWith(result);
      } catch (Exception e) {
        return PendingResult.completedExceptionallyWith(e);
      }
    }

    // We're in a background thread, delegate.
    logger.log(Level.DEBUG, "runLater Called from background thread {0}",
        Thread.currentThread().getName());
    return env.doRunLater(supplier);
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
   * Returns the BBjAPI instance for the current environment.
   *
   * @return the BBjAPI instance for the current environment.
   */

  public BBjAPI getBBjAPI() {
    return this.api;
  }

  /**
   * Returns the BBjSysGui instance for the current environment.
   *
   * @return the BBjSysGui instance for the current environment.
   */
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

  /**
   * Finds the Environment associated with the current thread.
   */
  private static Environment findEnvironmentForThread() {
    // First check if current thread has an Environment (UI thread)
    Environment env = getCurrent();
    if (env != null) {
      return env;
    }

    // For background threads, use the inherited Environment
    return inheritableEnvironment.get();
  }

  /**
   * Executes the given supplier in the Environment's thread context and returns a PendingResult.
   *
   * <p>
   * This method is called when the Environment is not the current thread, queuing the request to be
   * processed later in the Environment's thread.
   * </p>
   *
   * @param <T> the type of the result
   * @param supplier the supplier to execute
   *
   * @return a PendingResult that completes with the supplier's result
   */
  private <T> PendingResult<T> doRunLater(Supplier<T> supplier) {
    // We're in a background thread, queue the request
    int requestId = accessRequestCounter.incrementAndGet();
    PendingResult<T> result = new PendingResult<>();

    EnvironmentAccessRequest request = new EnvironmentAccessRequest(requestId, supplier, result);
    accessQueue.offer(request);

    // Store the pending result
    @SuppressWarnings("unchecked")
    PendingResult<Object> objectResult = (PendingResult<Object>) result;
    pendingResults.put(requestId, objectResult);

    logger.log(Level.DEBUG, "Queued runLater request {0} from thread {1}", requestId,
        Thread.currentThread().getName());

    return result;
  }

  /**
   * Creates and starts the runLater queue timer that processes queued requests.
   *
   * <p>
   * This method initializes a timer that checks the access queue every 50 milliseconds and
   * processes any pending requests by executing their suppliers in the Environment's thread.
   * </p>
   */
  private void createRunLaterQueueTimer() {
    // Only create timer if we have a valid BBjAPI (not in test environments)
    if (api == null) {
      logger.log(Level.DEBUG, "Skipping timer creation in test environment");
      return;
    }

    // Create interval timer that checks every 50ms
    laterQueueTimer = new Interval(0.05f, event -> {
      if (!accessQueue.isEmpty()) {
        logger.log(Level.DEBUG, "Timer tick, processing queue");
        processRunLaterQueue();
      }
    });

    laterQueueTimer.start();
    logger.log(Level.DEBUG, "runLater queue timer created and started");
  }

  /**
   * Returns the runLater queue timer.
   *
   * @return the runLater queue timer
   */
  Interval getLaterQueueTimer() {
    return laterQueueTimer;
  }

  /**
   * Processes the runLater queue, executing queued requests in the Environment's thread.
   *
   * <p>
   * This method is called by the runLater queue timer to process any pending requests. It executes
   * each request's supplier and completes the associated PendingResult.
   * </p>
   */
  void processRunLaterQueue() {
    int processedCount = 0;
    int cancelledCount = 0;
    EnvironmentAccessRequest request;
    while ((request = accessQueue.poll()) != null) {
      final EnvironmentAccessRequest req = request;

      // Check if the PendingResult has been cancelled
      if (req.getPendingResult().isCancelled()) {
        cancelledCount++;
        logger.log(Level.DEBUG, "runLater Skipping cancelled request {0}", req.getId());
        pendingResults.remove(req.getId());
        continue;
      }

      processedCount++;
      logger.log(Level.DEBUG, "runLater Processing request {0}", req.getId());
      try {
        Object result = req.getSupplier().get();
        req.getPendingResult().complete(result);
      } catch (Exception e) {
        logger.log(Level.ERROR, "runLater Request {0} failed: {1}", req.getId(), e.getMessage());
        req.getPendingResult().completeExceptionally(e);
      } finally {
        pendingResults.remove(req.getId());
      }
    }
    if (processedCount > 0 || cancelledCount > 0) {
      logger.log(Level.DEBUG, "runLater Processed {0} requests, skipped {1} cancelled",
          processedCount, cancelledCount);
    }
  }
}
