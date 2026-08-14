package com.webforj.mcp;

import com.github.victools.jsonschema.generator.OptionPreset;
import com.github.victools.jsonschema.generator.SchemaGenerator;
import com.github.victools.jsonschema.generator.SchemaGeneratorConfigBuilder;
import com.github.victools.jsonschema.generator.SchemaVersion;
import com.github.victools.jsonschema.module.jackson.JacksonOption;
import com.github.victools.jsonschema.module.jackson.JacksonSchemaModule;
import com.webforj.component.Component;
import com.webforj.mcp.annotation.McpApp;
import com.webforj.router.RoutePattern;
import java.util.Locale;
import java.util.Map;
import tools.jackson.databind.JsonNode;
import tools.jackson.databind.json.JsonMapper;

/**
 * Describes one view marked with {@link McpApp} together with the tool it projects to.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public final class McpAppDescriptor {

  private static final String ROOT_TOOL_NAME = "app";

  private final Class<? extends Component> componentClass;
  private final String route;
  private final String toolName;
  private final String description;
  private final String inputSchema;
  private final McpAppDisplayMode displayMode;

  /**
   * Creates a descriptor for a marked view.
   *
   * @param componentClass the view class carrying {@link McpApp}
   * @param route the route the view is registered under
   * @throws IllegalArgumentException if the class carries no {@link McpApp}, the description is
   *         blank, or the route takes required parameters
   */
  McpAppDescriptor(Class<? extends Component> componentClass, String route) {
    McpApp annotation = componentClass.getAnnotation(McpApp.class);
    if (annotation == null) {
      throw new IllegalArgumentException(
          "Class is not annotated with @McpApp: " + componentClass.getName());
    }

    if (annotation.description().isBlank()) {
      throw new IllegalArgumentException("@McpApp requires a description on class: "
          + componentClass.getName() + ". Hosts use it to decide when to open the view.");
    }

    String url = resolveParameterFreeUrl(componentClass, route);

    this.componentClass = componentClass;
    this.route = url.isEmpty() ? "/" : url;
    this.description = annotation.description();
    this.toolName = annotation.name().isBlank() ? toToolName(this.route) : annotation.name();
    this.inputSchema = resolveInputSchema(componentClass, annotation);
    this.displayMode = annotation.displayMode();
  }

  /**
   * Returns the marked view class.
   *
   * @return the view class
   */
  public Class<? extends Component> getComponentClass() {
    return componentClass;
  }

  /**
   * Returns the route the host navigates to when the tool runs.
   *
   * @return the route path
   */
  public String getRoute() {
    return route;
  }

  /**
   * Returns the name of the generated tool.
   *
   * @return the tool name
   */
  public String getToolName() {
    return toolName;
  }

  /**
   * Returns the description the host reads.
   *
   * @return the view description
   */
  public String getDescription() {
    return description;
  }

  /**
   * Returns the JSON Schema document the tool declares as its input, whether declared as a document
   * or generated from a declared class.
   *
   * @return the schema document, {@code null} when the tool takes no input
   */
  public String getInputSchema() {
    return inputSchema;
  }

  /**
   * Returns the display mode the view asks the host for when it opens.
   *
   * @return the display mode
   */
  public McpAppDisplayMode getDisplayMode() {
    return displayMode;
  }

  private static String resolveParameterFreeUrl(Class<? extends Component> componentClass,
      String route) {
    // The router's own pattern decides what the route needs. Required parameters refuse an empty
    // parameter map, while optional parameters, wildcards, and layout segments resolve to a
    // navigable URL without one.
    try {
      return new RoutePattern(route).generateUrl(Map.of());
    } catch (IllegalArgumentException e) {
      throw new IllegalArgumentException(
          "@McpApp cannot expose the route '" + route + "' on class " + componentClass.getName()
              + " because it takes required parameters. The generated tool takes no arguments, so"
              + " define a custom tool that supplies the parameters instead.",
          e);
    }
  }

  private static String resolveInputSchema(Class<? extends Component> componentClass,
      McpApp annotation) {
    boolean declaresDocument = !annotation.inputSchema().isBlank();
    boolean declaresClass = annotation.input() != Void.class;

    if (declaresDocument && declaresClass) {
      throw new IllegalArgumentException("@McpApp declares both input and inputSchema on class "
          + componentClass.getName() + ". Declare the schema in one form only.");
    }

    if (declaresClass) {
      return generateInputSchema(componentClass, annotation.input());
    }

    if (!declaresDocument) {
      return null;
    }

    // The document is published verbatim, so a schema the host cannot read must never leave the
    // deployment. Parsing at scan time turns a typo into a startup failure naming the view.
    JsonNode parsed;
    try {
      parsed = JsonMapper.shared().readTree(annotation.inputSchema());
    } catch (RuntimeException e) {
      throw new IllegalArgumentException("@McpApp declares an input schema that is not readable"
          + " JSON on class " + componentClass.getName(), e);
    }

    if (parsed == null || !parsed.isObject()) {
      throw new IllegalArgumentException("@McpApp declares an input schema whose root is not a"
          + " JSON object on class " + componentClass.getName());
    }

    return annotation.inputSchema();
  }

  private static String generateInputSchema(Class<? extends Component> componentClass,
      Class<?> input) {
    try {
      return InputSchemaGenerator.INSTANCE.generateSchema(input).toString();
    } catch (RuntimeException e) {
      throw new IllegalArgumentException("@McpApp declares an input class the schema cannot be"
          + " generated from on class " + componentClass.getName(), e);
    }
  }

  private static String toToolName(String route) {
    String trimmed = route.replaceAll("^/++", "").replaceAll("/++$", "");
    if (trimmed.isBlank()) {
      return ROOT_TOOL_NAME;
    }

    return trimmed.replace('/', '-').toLowerCase(Locale.ROOT);
  }

  // Built on first use so only views declaring an input class touch the generation stack. The
  // Jackson module resolves the descriptions from @JsonPropertyDescription and the required
  // properties from @JsonProperty(required = true).
  private static final class InputSchemaGenerator {
    private static final SchemaGenerator INSTANCE = new SchemaGenerator(
        new SchemaGeneratorConfigBuilder(SchemaVersion.DRAFT_2020_12, OptionPreset.PLAIN_JSON)
            .with(new JacksonSchemaModule(JacksonOption.RESPECT_JSONPROPERTY_REQUIRED)).build());

    private InputSchemaGenerator() {
      // Constant holder
    }
  }
}
