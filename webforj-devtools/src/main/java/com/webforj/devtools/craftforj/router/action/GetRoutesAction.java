package com.webforj.devtools.craftforj.router.action;

import com.google.gson.JsonObject;
import com.webforj.devtools.craftforj.action.CraftforjActionHandler;
import com.webforj.devtools.craftforj.router.RouteCollector;
import com.webforj.devtools.craftforj.router.model.RouteInfo;
import java.util.List;

/**
 * Action handler that returns all registered routes.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class GetRoutesAction implements CraftforjActionHandler<GetRoutesAction.Response> {

  /** The action name. */
  public static final String ACTION = "router.getRoutes";

  private final RouteCollector collector;

  /**
   * Creates a new GetRoutesAction with a default collector.
   */
  public GetRoutesAction() {
    this(new RouteCollector());
  }

  /**
   * Creates a new GetRoutesAction with the given collector.
   *
   * @param collector the route collector
   */
  public GetRoutesAction(RouteCollector collector) {
    this.collector = collector;
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
    List<RouteInfo> routes = collector.collectRoutes();
    return new Response(routes);
  }

  /**
   * Response containing the routes.
   */
  public static class Response {
    private final List<RouteInfo> routes;

    Response(List<RouteInfo> routes) {
      this.routes = routes;
    }

    /**
     * Gets the routes.
     *
     * @return the routes
     */
    public List<RouteInfo> getRoutes() {
      return routes;
    }
  }
}
