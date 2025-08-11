package com.webforj;

/**
 * Listener interface for App lifecycle events.
 *
 * <p>
 * Implementations of this interface will be automatically discovered and notified of key lifecycle
 * events during the execution of a webforj {@link App}. To control the order of listener execution,
 * annotate your implementation class with {@link com.webforj.annotation.AppListenerPriority}.
 * </p>
 *
 * <p>
 * Example usage:
 * </p>
 *
 * <pre>{@code
 * &#64;AppListenerPriority(5)
 * public class MyLifecycleListener implements AppLifecycleListener {
 *
 *   &#64;Override
 *   public void onWillRun(App app) {
 *     // Called before app.run()
 *   }
 *
 *   &#64;Override
 *   public void onDidRun(App app) {
 *     // Called after app.run()
 *   }
 * }
 * }</pre>
 *
 * <p>
 * All methods in this interface have default empty implementations, allowing implementations to
 * only override the methods they are interested in.
 * </p>
 *
 * @see com.webforj.annotation.AppListenerPriority
 * @see App
 *
 * @author Hyyan Abo Fakher
 * @since 25.02
 */
public interface AppLifecycleListener {

  /**
   * Called before the app is created, after environment is initialized.
   *
   * <p>
   * This method is called after the environment has been fully initialized and the configuration
   * has been loaded, but before the app class is instantiated. This is the ideal place to merge
   * external configuration sources or modify the configuration before the app is created.
   * </p>
   *
   * @param env the initialized environment with configuration
   * @since 25.03
   */
  default void onWillCreate(Environment env) {
    // Default empty implementation
  }

  /**
   * Called immediately after the app is created but before it runs.
   *
   * <p>
   * This method is called after the app instance has been created but before {@code onWillRun()}
   * and {@code run()} are invoked. This provides an early hook for app-level initialization that
   * needs to happen before the normal lifecycle begins.
   * </p>
   *
   * @param app the newly created app instance
   * @since 25.03
   */
  default void onDidCreate(App app) {
    // Default empty implementation
  }

  /**
   * Called before the app's {@code run()} method is invoked.
   *
   * <p>
   * This method is called after the app has been initialized but before the main application logic
   * in {@code run()} is executed.
   * </p>
   *
   * @param app the app instance that is about to run
   */
  default void onWillRun(App app) {
    // Default empty implementation
  }

  /**
   * Called after the app's {@code run()} method completes successfully.
   *
   * <p>
   * This method is called after the app's {@code run()} method has completed without throwing an
   * exception.
   * </p>
   *
   * @param app the app instance that has finished running
   */
  default void onDidRun(App app) {
    // Default empty implementation
  }

  /**
   * Called before the app is terminated.
   *
   * <p>
   * This method is called when the app is about to be terminated, either due to normal completion
   * or an error condition. It provides an opportunity to perform cleanup operations before the app
   * shuts down.
   * </p>
   *
   * @param app the app instance that is about to terminate
   */
  default void onWillTerminate(App app) {
    // Default empty implementation
  }

  /**
   * Called after the app is terminated.
   *
   * <p>
   * This method is called after the app has been terminated. At this point, app resources may no
   * longer be available, so this method should only perform minimal cleanup or logging operations.
   * </p>
   *
   * @param app the app instance that has been terminated
   */
  default void onDidTerminate(App app) {
    // Default empty implementation
  }
}
