package com.webforj.devtools.craftforj.action;

import com.google.gson.JsonObject;

/**
 * Interface for handling craftforJ actions.
 *
 * <p>
 * Each action handler is responsible for a specific action type (e.g., "getTree", "setProperty").
 * Handlers are registered with the {@link CraftforjActionRegistry} and dispatched based on the
 * action name in incoming requests.
 * </p>
 *
 * @param <T> the response type returned by this handler
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public interface CraftforjActionHandler<T> {

  /**
   * Gets the action name this handler responds to.
   *
   * @return the action name (e.g., "getTree", "setProperty")
   */
  String getAction();

  /**
   * Handles the action request.
   *
   * @param params the request parameters as JSON (may be null or empty)
   * @return the response object to be serialized to JSON
   */
  T handle(JsonObject params);
}
