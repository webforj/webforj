package com.webforj.devtools.craftforj.inspector.action;

import com.google.gson.JsonObject;
import com.webforj.devtools.craftforj.action.CraftforjActionHandler;
import com.webforj.devtools.craftforj.inspector.source.parser.SourceParserService;
import com.webforj.devtools.craftforj.model.ComponentMeta;
import com.webforj.devtools.craftforj.utilities.ComponentMapBuilder;
import java.util.List;
import java.util.Map;

/**
 * Action handler that returns component metadata.
 *
 * <p>
 * This action returns a flat map of component metadata keyed by clientId (dwc-id). The craftforJ
 * extension uses this map along with the DOM structure to build the component tree.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class GetComponentMapAction
    implements CraftforjActionHandler<GetComponentMapAction.Response> {

  /**
   * The action name for this handler.
   */
  public static final String ACTION = "inspector.getComponentMap";
  private final ComponentMapBuilder mapBuilder;

  /**
   * Creates a new GetComponentMapAction with a default map builder.
   */
  public GetComponentMapAction() {
    this(new ComponentMapBuilder(SourceParserService.getCurrent()));
  }

  /**
   * Creates a new GetComponentMapAction with the given map builder.
   *
   * @param mapBuilder the map builder to use
   */
  public GetComponentMapAction(ComponentMapBuilder mapBuilder) {
    this.mapBuilder = mapBuilder;
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
    Map<String, List<ComponentMeta>> components = mapBuilder.buildComponentMap();
    return new Response(components);
  }

  /**
   * Response containing the component map.
   *
   * @author Hyyan Abo Fakher
   * @since 26.02
   */
  public static class Response {

    private final Map<String, List<ComponentMeta>> components;

    /**
     * Creates a new response with the given component map.
     *
     * @param components map of clientId to list of ComponentMeta (compositeStack)
     */
    Response(Map<String, List<ComponentMeta>> components) {
      this.components = components;
    }

    /**
     * Gets the component map.
     *
     * @return map of clientId to list of ComponentMeta
     */
    public Map<String, List<ComponentMeta>> getComponents() {
      return components;
    }
  }
}
