package com.webforj;

import com.basis.bbj.proxies.event.BBjCustomEvent;

/**
 * Handles the "run later" event in the BBj environment.
 *
 * @author Hyyan Abo Fakher
 * @since 25.02
 */
public class EnvironmentRunLaterEventHandler {
  private final Environment env;

  /**
   * Constructs a new EnvironmentRunLaterEventHandler.
   *
   * @param env the Environment instance to handle the event for
   */
  EnvironmentRunLaterEventHandler(Environment env) {
    this.env = env;
  }

  /**
   * Handle the event.
   *
   * @param ev the BBjCustomEvent containing the request ID
   */
  public void handleEvent(BBjCustomEvent ev) {
    // Get the request ID from the event object
    String requestId = ev.getObject().toString();
    env.processRunLaterRequest(requestId);
  }
}

