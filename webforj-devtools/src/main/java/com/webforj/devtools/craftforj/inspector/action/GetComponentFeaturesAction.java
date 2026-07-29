package com.webforj.devtools.craftforj.inspector.action;

import com.google.gson.JsonObject;
import com.webforj.component.Component;
import com.webforj.devtools.craftforj.action.CraftforjActionException;
import com.webforj.devtools.craftforj.action.CraftforjActionHandler;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandlerRegistry;
import com.webforj.devtools.craftforj.inspector.model.FeatureGroup;
import com.webforj.devtools.craftforj.utilities.ComponentLocator;
import java.util.List;
import java.util.Optional;

/**
 * Action handler that returns features for a specific component.
 *
 * <p>
 * This action returns the component's supported features and their current values. The client uses
 * this information to render appropriate panels for editing component properties.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class GetComponentFeaturesAction
    implements CraftforjActionHandler<GetComponentFeaturesAction.Response> {

  /**
   * The action name for this handler.
   */
  public static final String ACTION = "inspector.getComponentFeatures";

  private static final String PARAM_PARENT = "parent";
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
  public Response handle(JsonObject params) {
    String id = params.has("id") ? params.get("id").getAsString() : null;

    if (id == null || id.isEmpty()) {
      throw new CraftforjActionException("id is required");
    }

    Optional<Component> componentOpt = ComponentLocator.findById(id);
    if (componentOpt.isEmpty()) {
      throw new CraftforjActionException("Component not found: " + id);
    }

    // Extract parent type if provided
    // For composites, use compositeComponentType (the bound component type)
    String parentType = null;
    if (params.has(PARAM_PARENT) && params.get(PARAM_PARENT).isJsonObject()) {
      JsonObject parent = params.getAsJsonObject(PARAM_PARENT);
      if (parent.has("compositeComponentType")) {
        parentType = parent.get("compositeComponentType").getAsString();
      } else if (parent.has("componentType")) {
        parentType = parent.get("componentType").getAsString();
      }
    }

    Component component = componentOpt.get();
    List<FeatureGroup> featureGroups = registry.getFeatureGroups(component, parentType);

    return new Response(id, featureGroups);
  }

  /**
   * Response containing component features.
   */
  public static class Response {

    private final String id;
    private final List<FeatureGroup> featureGroups;

    Response(String id, List<FeatureGroup> featureGroups) {
      this.id = id;
      this.featureGroups = featureGroups;
    }

    /**
     * Gets the component id.
     *
     * @return the id
     */
    public String getId() {
      return id;
    }

    /**
     * Gets the feature groups.
     *
     * @return the feature groups
     */
    public List<FeatureGroup> getFeatureGroups() {
      return featureGroups;
    }
  }
}
