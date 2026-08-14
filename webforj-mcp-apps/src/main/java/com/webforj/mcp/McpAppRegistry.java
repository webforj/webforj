package com.webforj.mcp;

import com.webforj.component.Component;
import com.webforj.mcp.annotation.McpApp;
import com.webforj.router.RouteEntry;
import com.webforj.router.RouteRegistry;
import io.modelcontextprotocol.server.McpServerFeatures.SyncToolSpecification;
import io.modelcontextprotocol.spec.McpSchema.CallToolResult;
import io.modelcontextprotocol.spec.McpSchema.Tool;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import tools.jackson.core.type.TypeReference;
import tools.jackson.databind.JsonNode;
import tools.jackson.databind.json.JsonMapper;

/**
 * Projects the views marked with {@link McpApp} onto MCP tool specifications.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public final class McpAppRegistry {

  private static final Map<String, Object> NO_ARGUMENTS_SCHEMA =
      Map.of("type", "object", "properties", Map.of());
  private static final TypeReference<Map<String, Object>> SCHEMA_MAP = new TypeReference<>() {};
  private static final String ROUTE_FIELD = "_route";
  private static final String DISPLAY_MODE_FIELD = "_displayMode";

  private final List<McpAppDescriptor> descriptors;

  private McpAppRegistry(List<McpAppDescriptor> descriptors) {
    this.descriptors = descriptors;
  }

  /**
   * Reads the marked views out of a route registry.
   *
   * @param registry the registry the router builds from the application routes
   * @return the registry over the marked views
   * @throws IllegalStateException if two marked views claim the same tool name
   */
  public static McpAppRegistry ofRegistry(RouteRegistry registry) {
    // Sorted by tool name so the host sees a stable list. The registry orders entries by route
    // matching precedence, which is an ordering the tool list has no use for.
    Map<String, McpAppDescriptor> byToolName = new TreeMap<>();

    for (RouteEntry entry : registry.getAvailableRouteEntires()) {
      Class<? extends Component> component = entry.getComponent();
      if (component == null || !component.isAnnotationPresent(McpApp.class)) {
        continue;
      }

      McpAppDescriptor descriptor = new McpAppDescriptor(component, entry.getPath());
      McpAppDescriptor clash = byToolName.put(descriptor.getToolName(), descriptor);
      if (clash != null) {
        throw new IllegalStateException("Two views claim the MCP tool name '"
            + descriptor.getToolName() + "': " + clash.getComponentClass().getName() + " and "
            + component.getName() + ". Set a distinct name on one of them.");
      }
    }

    return new McpAppRegistry(List.copyOf(byToolName.values()));
  }

  /**
   * Scans the given packages for routes and reads the marked views out of the result.
   *
   * @param packages the packages the application routes live in
   * @return the registry over the marked views
   * @throws IllegalStateException if two marked views claim the same tool name
   */
  public static McpAppRegistry ofPackages(String[] packages) {
    return ofRegistry(RouteRegistry.ofPackage(packages));
  }

  /**
   * Returns the marked views, ordered by tool name.
   *
   * @return the view descriptors
   */
  public List<McpAppDescriptor> getDescriptors() {
    return descriptors;
  }

  /**
   * Returns one tool specification per marked view.
   *
   * <p>
   * Each tool takes no arguments, points at the app resource through its meta, and answers with the
   * route of its view.
   * </p>
   *
   * @return the tool specifications
   */
  public List<SyncToolSpecification> getToolSpecifications() {
    List<SyncToolSpecification> specifications = new ArrayList<>(descriptors.size());
    for (McpAppDescriptor descriptor : descriptors) {
      specifications.add(toSpecification(descriptor));
    }

    return List.copyOf(specifications);
  }

  private static SyncToolSpecification toSpecification(McpAppDescriptor descriptor) {
    Tool tool = Tool.builder(descriptor.getToolName(), toSchemaMap(descriptor))
        .description(descriptor.getDescription()).meta(toolMeta(descriptor)).build();

    CallToolResult result = CallToolResult.builder().addTextContent(descriptor.getDescription())
        .structuredContent(toStructuredContent(descriptor)).build();

    return new SyncToolSpecification(tool, (exchange, request) -> result);
  }

  private static Map<String, Object> toStructuredContent(McpAppDescriptor descriptor) {
    // The app page reads the route to navigate and forwards the declared display mode to the host.
    Map<String, Object> structured = new LinkedHashMap<>();
    structured.put(ROUTE_FIELD, descriptor.getRoute());
    structured.put(DISPLAY_MODE_FIELD, descriptor.getDisplayMode().getValue());

    return structured;
  }

  private static Map<String, Object> toSchemaMap(McpAppDescriptor descriptor) {
    if (descriptor.getInputSchema() == null) {
      return NO_ARGUMENTS_SCHEMA;
    }

    // The declared document goes in verbatim, read into the map form the SDK accepts. The
    // descriptor already proved the document parses, so this read cannot fail here.
    JsonNode schema = JsonMapper.shared().readTree(descriptor.getInputSchema());
    return JsonMapper.shared().convertValue(schema, SCHEMA_MAP);
  }

  private static Map<String, Object> toolMeta(McpAppDescriptor descriptor) {
    // Every tool points at the resource of its own route, so the page a host renders for it
    // opens the application directly at the view the tool projects.
    return Map.of("ui", Map.of("resourceUri", McpAppResource.getUriOf(descriptor.getRoute())));
  }
}
