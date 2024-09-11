package com.webforj.router;

import com.google.gson.Gson;
import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import com.webforj.Environment;
import com.webforj.Page;
import com.webforj.annotation.Experimental;
import com.webforj.component.Component;
import com.webforj.router.history.Location;
import com.webforj.router.history.ParametersBag;
import java.util.List;
import java.util.Map;

/**
 * Utility class for working with routes.
 *
 * @author Hyyan Abo Fakher
 * @since 24.12
 */
@Experimental(since = "24.12",
    warning = "This class is experimental and may be removed anytime in future versions.")
public final class RouterDevUtils {

  private RouterDevUtils() {
    // no-op
  }

  /**
   * Dumps the routes to the browser console.
   *
   * @param routes The list of routes to dump.
   */
  public static void logRouteEntires(List<RouteEntry> routes) {
    if (!Environment.getCurrent().isDebug()) {
      return;
    }

    Gson gson = new Gson();
    JsonArray jsonArray = new JsonArray();

    for (RouteEntry route : routes) {
      JsonObject routeJson = new JsonObject();
      routeJson.addProperty("path", route.getPath());
      routeJson.addProperty("component", route.getComponent().getName());
      routeJson.addProperty("outlet",
          route.getOutlet() != null ? route.getOutlet().getName() : "None");
      routeJson.addProperty("FrameId", route.getFrameId().map(String::valueOf).orElse("None"));
      routeJson.addProperty("priority", route.getPriority());
      jsonArray.add(routeJson);
    }

    String json = gson.toJson(jsonArray);
    String js = "(function(routesData) {" + "   routesData.forEach(function(route) {"
        + "       console.groupCollapsed('%cRoute:%c ' + route.path, 'background: #4c47ff; color: white; padding: 2px 6px; border-radius: 5px;', 'color: inherit');"
        + "       console.log('Path      :', route.path);"
        + "       console.log('Component :', route.component);"
        + "       console.log('Outlet    :', route.outlet);"
        + "       console.log('Frame ID  :', route.FrameId);"
        + "       console.log('Priority  :', route.priority);" + "       console.groupEnd();"
        + "   });"
        + "   console.log('%c Total routes: %c ' + routesData.length, 'background: #28a745; color: white; padding: 2px 6px; border-radius: 5px;', 'color: inherit');"
        + "})(" + json + ");";

    Page.getCurrent().executeJsVoidAsync(js);
  }

  /**
   * Logs a navigation action to the browser console.
   *
   * @param context The navigation context.
   * @param component The component to navigate to.
   */
  public static void logNavigationAction(NavigationContext context,
      Class<? extends Component> component) {
    if (!Environment.getCurrent().isDebug()) {
      return;
    }

    Location location = context.getLocation();
    NavigationOptions options = context.getOptions().get();
    ParametersBag routeParameters = context.getRouteParameters();

    StringBuilder js = new StringBuilder();

    js.append("(function() {").append("   console.groupCollapsed('%cNavigation: %c ' + '")
        .append(location.getFullURI())
        .append(
            "', 'background: #008080; color: white; padding: 2px 6px; border-radius: 5px;', 'color: inherit');")
        .append("   console.log('Component :', '").append(component.getName()).append("');")

        // Navigation Options group
        .append("   console.groupCollapsed('Options');")
        .append("   console.log('Type               :', '").append(options.getNavigationType())
        .append("');").append("   console.log('Fire Events        :', ")
        .append(options.isFireEvents()).append(");")
        .append("   console.log('Invoke Observers   :', ").append(options.isInvokeObservers())
        .append(");").append("   console.log('Update History     :', ")
        .append(options.isUpdateHistory()).append(");").append("   console.groupEnd();"); // NOSONAR

    js.append("   console.groupCollapsed('Parameters');");

    for (Map.Entry<String, String> param : routeParameters) {
      js.append("   console.log('").append(param.getKey()).append(" : ', '")
          .append(param.getValue()).append("');");
    }

    js.append("   console.groupEnd();") // Close the parameters group
        .append("   console.groupEnd();") // Close the main group
        .append("})();");

    Page.getCurrent().executeJsVoidAsync(js.toString());
  }
}
