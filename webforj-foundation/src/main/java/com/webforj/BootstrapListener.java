package com.webforj;

import com.webforj.annotation.BootstrapListenerPriority;

/**
 * Listener interface for Bootstrap lifecycle events.
 *
 * <p>
 * Implementations of this interface will be automatically discovered and notified of key lifecycle
 * events during the webforJ bootstrap process. The bootstrap process is responsible for
 * initializing the environment, loading configuration, discovering the application entry point, and
 * creating the App instance.
 * </p>
 *
 * <p>
 * To control the order of listener execution, annotate your implementation class with
 * {@link BootstrapListenerPriority}.
 * </p>
 *
 * <p>
 * Example usage:
 * </p>
 *
 * <pre>{@code
 * &#64;BootstrapListenerPriority(5)
 * public class CustomBootstrapListener implements BootstrapListener {
 *
 *   &#64;Override
 *   public void onEnvironmentPrepared(BootstrapContext context) {
 *     // Modify or enhance configuration before it's processed
 *     Config customConfig = loadCustomConfiguration();
 *     context.mergeConfiguration(customConfig);
 *   }
 *
 *   &#64;Override
 *   public void onAppCreated(BootstrapContext context, App app) {
 *     // Perform initialization after the App is created
 *     initializeCustomServices(app);
 *   }
 * }
 * }</pre>
 *
 * <p>
 * All methods in this interface have default empty implementations, allowing implementations to
 * only override the methods they are interested in.
 * </p>
 *
 * @see BootstrapListenerPriority
 * @see BootstrapContext
 * @see Bootstrap
 *
 * @author Hyyan Abo Fakher
 * @since 25.03
 */
public interface BootstrapListener {

  /**
   * Called when the bootstrap process starts.
   *
   * <p>
   * This is the earliest hook in the bootstrap lifecycle. At this point, only the basic bootstrap
   * parameters are available. The Environment has not yet been initialized.
   * </p>
   *
   * <p>
   * Use this hook for:
   * </p>
   * <ul>
   * <li>Early logging or metrics initialization</li>
   * <li>Setting up resources that must be available before Environment initialization</li>
   * <li>Validating prerequisites or system requirements</li>
   * </ul>
   *
   * @param context the bootstrap context containing initialization parameters
   */
  default void onStarting(BootstrapContext context) {
    // no-op
  }

  /**
   * Called after the Environment is initialized but before configuration is processed.
   *
   * <p>
   * This is the primary hook for configuration customization. The Environment has been initialized
   * and the default configuration has been loaded, but the configuration has not yet been processed
   * or applied.
   * </p>
   *
   * <p>
   * Use this hook for:
   * </p>
   * <ul>
   * <li>Injecting external configuration (e.g., from Spring, or system properties)</li>
   * <li>Modifying or overriding default configuration values</li>
   * <li>Adding custom configuration sources</li>
   * <li>Validating configuration before it's processed</li>
   * </ul>
   *
   * @param context the bootstrap context with access to the loaded configuration
   */
  default void onEnvironmentPrepared(BootstrapContext context) {
    // no-op
  }

  /**
   * Called after configuration is processed and the App class has been detected.
   *
   * <p>
   * At this point, the configuration has been fully processed and the application entry point class
   * has been determined (either from configuration, annotation scanning, or explicit
   * specification).
   * </p>
   *
   * <p>
   * Use this hook for:
   * </p>
   * <ul>
   * <li>Performing actions based on the final configuration</li>
   * <li>Setting up resources based on the detected App class</li>
   * <li>Registering services that depend on configuration</li>
   * <li>Validating the application setup before App creation</li>
   * </ul>
   *
   * @param context the bootstrap context with processed configuration and detected App class
   */
  default void onContextPrepared(BootstrapContext context) {
    // no-op
  }

  /**
   * Called after the App instance is created but before it is initialized.
   *
   * <p>
   * The App instance has been instantiated but its {@code initialize()} method has not yet been
   * called. This is the last opportunity to modify the App instance before it starts its own
   * lifecycle.
   * </p>
   *
   * <p>
   * Use this hook for:
   * </p>
   * <ul>
   * <li>Registering App-specific services or handlers</li>
   * <li>Setting up aspect-oriented programming or proxies</li>
   * <li>Performing final validation before App initialization</li>
   * </ul>
   *
   * @param context the bootstrap context
   * @param app the newly created App instance
   */
  default void onAppCreated(BootstrapContext context, App app) {
    // no-op
  }

  /**
   * Called when the bootstrap process fails.
   *
   * <p>
   * This method is called if an exception occurs at any point during the bootstrap process. It
   * provides an opportunity for notification and cleanup only - the bootstrap failure cannot be
   * recovered from this hook. After all listeners are notified, the original exception will be
   * re-thrown and the bootstrap will terminate.
   * </p>
   *
   * <p>
   * Note that the bootstrap state may be incomplete depending on where the failure occurred. For
   * example, if the failure happens during environment initialization, the configuration will not
   * be available in the context.
   * </p>
   *
   * <p>
   * <strong>Important:</strong> Any exceptions thrown from this method will be logged but will not
   * prevent other listeners from being notified, and will not replace the original bootstrap
   * failure exception.
   * </p>
   *
   * <p>
   * Use this hook for:
   * </p>
   * <ul>
   * <li>Logging additional error context or diagnostics</li>
   * <li>Cleaning up partially initialized resources</li>
   * <li>Sending error notifications or alerts</li>
   * <li>Recording metrics about the failure</li>
   * </ul>
   *
   * @param context the bootstrap context at the point of failure
   * @param exception the exception that caused the failure
   */
  default void onFailed(BootstrapContext context, Throwable exception) {
    // no-op
  }

  /**
   * Called when the bootstrap is being cleaned up.
   *
   * <p>
   * This method is called when the Environment is being cleaned up, either after successful
   * completion when the application terminates, or after a bootstrap failure. It provides listeners
   * with an opportunity to release any resources they may have allocated during the bootstrap
   * process.
   * </p>
   *
   * <p>
   * <strong>Important:</strong> Any exceptions thrown from this method will be logged but will not
   * prevent other listeners from being notified of cleanup.
   * </p>
   *
   * <p>
   * Use this hook for:
   * </p>
   * <ul>
   * <li>Closing connections or streams opened during bootstrap</li>
   * <li>Releasing temporary resources</li>
   * <li>Clearing caches or temporary data</li>
   * <li>Unregistering services or handlers</li>
   * </ul>
   *
   * @param context the bootstrap context
   */
  default void onCleanup(BootstrapContext context) {
    // no-op
  }
}
