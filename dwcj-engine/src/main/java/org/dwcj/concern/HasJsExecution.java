package org.dwcj.concern;

import org.dwcj.PendingResult;

/**
 * Interface for executing JavaScript code within a client-side context.
 *
 * <p>
 * This interface offers functionality to execute JavaScript scripts either synchronously or
 * asynchronously. It enables interactive capabilities with client-side environments, facilitating
 * dynamic script execution and manipulation of the execution context.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 23.05
 */
public interface HasJsExecution {

  /**
   * Executes a JavaScript script synchronously and returns the result.
   *
   * <p>
   * Executes the provided JavaScript code within the current context and returns the result. If the
   * execution context (such as a component) is not ready (e.g., not attached to the DOM), the
   * execution may be deferred. The {@code component} keyword within the script can access the
   * current context if applicable. In cases where immediate execution is not possible, the method
   * may return {@code null}.
   * </p>
   *
   * <p>
   * The results of the script execution are converted to corresponding Java types as follows:
   * <ul>
   * <li>JavaScript numbers to {@code java.lang.Integer}, {@code java.lang.Long}, or
   * {@code java.lang.Double}.</li>
   * <li>JavaScript strings to {@code java.lang.String}.</li>
   * <li>JavaScript booleans to {@code java.lang.Boolean}.</li>
   * <li>JavaScript null or undefined to {@code null}.</li>
   * <li>All other JavaScript types to their string representations.</li>
   * </ul>
   * </p>
   *
   * @param js The JavaScript code to be executed.
   * @return The result of the script execution, or {@code null} if immediate execution is not
   *         possible.
   */
  public Object executeJs(String js);

  /**
   * Executes a JavaScript script asynchronously.
   *
   * <p>
   * Initiates asynchronous execution of the provided JavaScript code. The method returns
   * immediately with an {@link PendingResult} that will be completed once the script execution is
   * finished. Similar to synchronous execution, if the execution context (such as a component) is
   * not ready, the script execution is queued until the context is available. The script has access
   * to the current context through the {@code component} keyword, if applicable.
   * </p>
   *
   * <p>
   * The results of the script execution are converted to corresponding Java types as follows:
   * <ul>
   * <li>JavaScript numbers to {@code java.lang.Integer}, {@code java.lang.Long}, or
   * {@code java.lang.Double}.</li>
   * <li>JavaScript strings to {@code java.lang.String}.</li>
   * <li>JavaScript booleans to {@code java.lang.Boolean}.</li>
   * <li>JavaScript null or undefined to {@code null}.</li>
   * <li>All other JavaScript types to their string representations.</li>
   * </ul>
   * </p>
   *
   * @param js The JavaScript code to execute asynchronously.
   * @return An {@link PendingResult} object representing the pending result of the script
   *         execution.
   */
  public PendingResult<Object> executeJsAsync(String js);
}
