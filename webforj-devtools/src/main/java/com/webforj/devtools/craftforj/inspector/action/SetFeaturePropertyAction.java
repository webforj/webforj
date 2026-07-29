package com.webforj.devtools.craftforj.inspector.action;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.webforj.component.Component;
import com.webforj.devtools.craftforj.action.CraftforjActionException;
import com.webforj.devtools.craftforj.action.CraftforjActionHandler;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandlerRegistry;
import com.webforj.devtools.craftforj.utilities.ComponentLocator;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

/**
 * Action handler that applies property changes to a component.
 *
 * <p>
 * This action receives property changes from the client and applies them to the component using the
 * appropriate feature handler.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class SetFeaturePropertyAction implements CraftforjActionHandler<Void> {

  /**
   * The action name for this handler.
   */
  public static final String ACTION = "inspector.setFeatureProperty";

  private final FeatureHandlerRegistry registry = new FeatureHandlerRegistry();

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
    String id = params.has("id") ? params.get("id").getAsString() : null;
    String featureType = params.has("featureType") ? params.get("featureType").getAsString() : null;

    if (id == null || id.isEmpty()) {
      throw new CraftforjActionException("id is required");
    }

    if (featureType == null || featureType.isEmpty()) {
      throw new CraftforjActionException("featureType is required");
    }

    Optional<Component> componentOpt = ComponentLocator.findById(id);
    if (componentOpt.isEmpty()) {
      throw new CraftforjActionException("Component not found: " + id);
    }

    JsonElement valueElement = params.has("value") ? params.get("value") : null;
    Component component = componentOpt.get();
    Object value = convertJsonValue(valueElement);

    // Parent-scoped features (layout item properties) apply through the parent's API; the parent
    // is resolved by the client, which owns the component tree
    Component parent = null;
    if (params.has("parentId") && !params.get("parentId").isJsonNull()) {
      String parentId = params.get("parentId").getAsString();
      if (parentId != null && !parentId.isEmpty()) {
        parent = ComponentLocator.findById(parentId).orElse(null);
      }
    }

    boolean success = registry.applyChange(component, parent, featureType, value);

    if (!success) {
      throw new CraftforjActionException("Failed to apply change for feature: " + featureType);
    }

    return null;
  }

  /**
   * Converts a JsonElement to a Java object for use by feature handlers.
   */
  private Object convertJsonValue(JsonElement element) {
    if (element == null || element.isJsonNull()) {
      return null;
    }

    if (element.isJsonPrimitive()) {
      var primitive = element.getAsJsonPrimitive();
      if (primitive.isBoolean()) {
        return primitive.getAsBoolean();
      }
      if (primitive.isNumber()) {
        return primitive.getAsNumber();
      }

      return primitive.getAsString();
    }

    if (element.isJsonObject()) {
      Map<String, Object> map = new HashMap<>();
      for (var entry : element.getAsJsonObject().entrySet()) {
        map.put(entry.getKey(), convertJsonValue(entry.getValue()));
      }

      return map;
    }

    if (element.isJsonArray()) {
      return element.getAsJsonArray().asList().stream().map(this::convertJsonValue).toList();
    }

    return element.toString();
  }
}
