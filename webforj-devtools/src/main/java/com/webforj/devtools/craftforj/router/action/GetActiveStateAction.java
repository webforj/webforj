package com.webforj.devtools.craftforj.router.action;

import com.google.gson.JsonObject;
import com.webforj.devtools.craftforj.action.CraftforjActionHandler;
import com.webforj.devtools.craftforj.router.ActiveRouteTracker;
import com.webforj.devtools.craftforj.router.model.ActiveRouteState;

/**
 * Action handler that returns the current active route state.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class GetActiveStateAction implements CraftforjActionHandler<GetActiveStateAction.Response> {

  /** The action name. */
  public static final String ACTION = "router.getActiveState";

  private final ActiveRouteTracker tracker;

  /**
   * Creates a new action with the given tracker.
   *
   * @param tracker the active route tracker
   */
  public GetActiveStateAction(ActiveRouteTracker tracker) {
    this.tracker = tracker;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getAction() {
    return ACTION;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Response handle(JsonObject params) {
    return new Response(tracker.getCurrentState());
  }

  /**
   * Response containing the active state.
   */
  public static class Response {
    private final ActiveRouteState state;

    Response(ActiveRouteState state) {
      this.state = state;
    }

    /**
     * Gets the state.
     *
     * @return the state
     */
    public ActiveRouteState getState() {
      return state;
    }
  }
}
