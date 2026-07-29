package com.webforj.devtools.craftforj.router.action;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.webforj.component.Component;
import com.webforj.devtools.craftforj.action.CraftforjActionException;
import com.webforj.devtools.craftforj.action.CraftforjActionHandler;
import com.webforj.devtools.craftforj.inspector.source.resolver.SourceFileResolver;
import com.webforj.devtools.craftforj.router.RouteComponentResolver;
import com.webforj.devtools.craftforj.router.RouteSecurityModifier;
import com.webforj.devtools.craftforj.router.model.SecurityAccess;
import com.webforj.router.Router;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

/**
 * Writes a security annotation into a route class source file.
 *
 * <p>
 * The component type must be a registered route and its source file is resolved server-side, never
 * taken from the client. The change takes effect after the application recompiles.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class SetRouteSecurityAction implements CraftforjActionHandler<Void> {

  /** The action name. */
  public static final String ACTION = "router.setSecurity";

  private final RouteSecurityModifier modifier;

  /** Creates the action with a default modifier. */
  public SetRouteSecurityAction() {
    this(new RouteSecurityModifier());
  }

  /**
   * Creates the action with a specific modifier.
   *
   * @param modifier the route security modifier
   */
  public SetRouteSecurityAction(RouteSecurityModifier modifier) {
    this.modifier = modifier;
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
  public Void handle(JsonObject params) {
    Router router = Router.getCurrent();
    if (router == null) {
      throw new CraftforjActionException("No router available");
    }

    String componentType =
        params.has("componentType") ? params.get("componentType").getAsString() : null;
    if (componentType == null || componentType.isEmpty()) {
      throw new CraftforjActionException("componentType is required");
    }

    SecurityAccess access = parseAccess(params);
    List<String> roles = parseRoles(params);

    Class<? extends Component> clazz = RouteComponentResolver.resolve(router, componentType);
    String sourceFile = SourceFileResolver.resolve(clazz.getName(), SourceFileResolver.JAVA_ONLY);
    if (sourceFile == null) {
      throw new CraftforjActionException("Java source file not found for: " + componentType);
    }

    modifier.apply(Path.of(sourceFile), clazz.getSimpleName(), access, roles);

    return null;
  }

  private SecurityAccess parseAccess(JsonObject params) {
    String access = params.has("access") ? params.get("access").getAsString() : null;
    if (access == null || access.isEmpty()) {
      throw new CraftforjActionException("access is required");
    }

    try {
      return SecurityAccess.valueOf(access);
    } catch (IllegalArgumentException e) {
      throw new CraftforjActionException("Unknown access: " + access);
    }
  }

  private List<String> parseRoles(JsonObject params) {
    if (!params.has("roles") || !params.get("roles").isJsonArray()) {
      return List.of();
    }

    List<String> roles = new ArrayList<>();
    JsonArray array = params.getAsJsonArray("roles");
    for (JsonElement element : array) {
      String role = element.getAsString();
      if (role.isEmpty()) {
        continue;
      }

      if (role.contains("\"") || role.contains("\\") || role.contains("\n")
          || role.contains("\r")) {
        throw new CraftforjActionException("Invalid role name: " + role);
      }

      roles.add(role);
    }

    return roles;
  }
}
