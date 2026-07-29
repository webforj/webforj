package com.webforj.devtools.craftforj.router.action;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.webforj.component.Component;
import com.webforj.devtools.craftforj.action.CraftforjActionException;
import com.webforj.devtools.craftforj.action.CraftforjActionHandler;
import com.webforj.devtools.craftforj.router.RouteComponentResolver;
import com.webforj.router.Router;
import com.webforj.router.history.Location;
import com.webforj.router.history.ParametersBag;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * Action handler that navigates to a specified route.
 *
 * <p>
 * Supports two navigation modes:
 * </p>
 * <ul>
 * <li>By path: provide a {@code path} parameter (e.g., "/products/123")</li>
 * <li>By component: provide {@code componentType} and optional {@code params}</li>
 * </ul>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class NavigateToRouteAction implements CraftforjActionHandler<Void> {

  /** The action name. */
  public static final String ACTION = "router.navigate";

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
  public Void handle(JsonObject params) {
    Router router = Router.getCurrent();
    if (router == null) {
      throw new CraftforjActionException("No router available");
    }

    // Check for path-based navigation first
    String path = params.has("path") ? params.get("path").getAsString() : null;
    if (path != null && !path.isEmpty()) {
      navigateByPath(router, path);
      return null;
    }

    // Fall back to component-based navigation
    String componentType =
        params.has("componentType") ? params.get("componentType").getAsString() : null;

    if (componentType == null || componentType.isEmpty()) {
      throw new CraftforjActionException("Either path or componentType is required");
    }

    Class<? extends Component> clazz = RouteComponentResolver.resolve(router, componentType);
    ParametersBag parametersBag = buildParametersBag(params.getAsJsonObject("params"));
    router.navigate(clazz, parametersBag);

    return null;
  }

  private void navigateByPath(Router router, String path) {
    Location location = new Location(path);
    router.navigate(location);
  }

  private ParametersBag buildParametersBag(JsonObject paramValues) {
    if (paramValues == null || paramValues.size() == 0) {
      return new ParametersBag();
    }

    Map<String, String> paramsMap = new LinkedHashMap<>();
    for (String key : paramValues.keySet()) {
      JsonElement value = paramValues.get(key);
      if (value != null && !value.isJsonNull()) {
        paramsMap.put(key, value.getAsString());
      }
    }

    return ParametersBag.of(paramsMap);
  }
}
