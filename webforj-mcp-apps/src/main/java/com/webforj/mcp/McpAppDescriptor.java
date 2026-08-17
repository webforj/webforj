package com.webforj.mcp;

import com.webforj.component.Component;
import com.webforj.mcp.annotation.McpApp;
import com.webforj.mcp.annotation.McpAppAction;
import com.webforj.mcp.annotation.McpAppInput;
import com.webforj.router.RoutePattern;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.TreeMap;
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
  private final List<McpAppActionDescriptor> actionDescriptors;
  private final McpAppMethodDescriptor openingInputMethod;

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
    this.toolName = resolveToolName(componentClass, route);
    this.actionDescriptors = createActionDescriptors(componentClass, annotation, this.toolName);
    this.openingInputMethod = findOpeningInputMethod(componentClass, annotation);
    this.inputSchema = resolveInputSchema(componentClass, annotation, this.openingInputMethod);
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

  List<McpAppActionDescriptor> getActionDescriptors() {
    return actionDescriptors;
  }

  McpAppMethodDescriptor getOpeningInputMethod() {
    return openingInputMethod;
  }

  static String resolveToolName(Class<? extends Component> componentClass, String route) {
    McpApp annotation = componentClass.getAnnotation(McpApp.class);
    if (annotation == null) {
      throw new IllegalArgumentException(
          "Class is not annotated with @McpApp: " + componentClass.getName());
    }

    if (!annotation.name().isBlank()) {
      return annotation.name();
    }

    String url = resolveParameterFreeUrl(componentClass, route);
    return toToolName(url.isEmpty() ? "/" : url);
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

  static McpAppMethodDescriptor resolveOpeningInputMethod(Class<? extends Component> viewClass) {
    McpApp annotation = viewClass.getAnnotation(McpApp.class);
    if (annotation == null) {
      return null;
    }

    return findOpeningInputMethod(viewClass, annotation);
  }

  private static List<McpAppActionDescriptor> createActionDescriptors(
      Class<? extends Component> viewType, McpApp annotation, String appToolName) {
    Map<String, McpAppActionDescriptor> actionsBySegment = new TreeMap<>();

    for (Class<?> ownerType : getMethodOwnerTypes(viewType, annotation)) {
      for (Method actionMethod : getDeclaredMethodsInStableOrder(ownerType)) {
        if (!actionMethod.isAnnotationPresent(McpAppAction.class)) {
          continue;
        }

        McpAppActionDescriptor action =
            new McpAppActionDescriptor(appToolName, viewType, actionMethod, ownerType);
        McpAppActionDescriptor duplicate = actionsBySegment.put(action.getNameSegment(), action);
        if (duplicate != null) {
          throw new IllegalArgumentException("Two actions of " + viewType.getName()
              + " claim the tool name '" + action.getToolName() + "': "
              + McpAppMethodDescriptor.describeMethod(duplicate.getInvocationMethod()) + " and "
              + McpAppMethodDescriptor.describeMethod(actionMethod) + ".");
        }
      }
    }

    return List.copyOf(actionsBySegment.values());
  }

  private static McpAppMethodDescriptor findOpeningInputMethod(Class<? extends Component> viewType,
      McpApp annotation) {
    McpAppMethodDescriptor inputMethod = null;

    for (Class<?> ownerType : getMethodOwnerTypes(viewType, annotation)) {
      for (Method candidate : getDeclaredMethodsInStableOrder(ownerType)) {
        if (!candidate.isAnnotationPresent(McpAppInput.class)) {
          continue;
        }

        if (inputMethod != null) {
          throw new IllegalArgumentException(
              "The view " + viewType.getName() + " declares more than one @McpAppInput method: "
                  + McpAppMethodDescriptor.describeMethod(inputMethod.getInvocationMethod())
                  + " and " + McpAppMethodDescriptor.describeMethod(candidate) + ".");
        }

        inputMethod = new McpAppMethodDescriptor(viewType, candidate, ownerType);
        if (inputMethod.getInputType() == null) {
          throw new IllegalArgumentException(
              "The @McpAppInput method " + McpAppMethodDescriptor.describeMethod(candidate)
                  + " must declare an object input parameter.");
        }
      }
    }

    return inputMethod;
  }

  private static List<Class<?>> getMethodOwnerTypes(Class<? extends Component> componentClass,
      McpApp annotation) {
    List<Class<?>> ownerTypes = new ArrayList<>();
    ownerTypes.add(componentClass);
    ownerTypes.addAll(List.of(annotation.actions()));

    return ownerTypes;
  }

  private static List<Method> getDeclaredMethodsInStableOrder(Class<?> ownerType) {
    return Arrays.stream(ownerType.getDeclaredMethods())
        .sorted(Comparator.comparing(Method::toGenericString)).toList();
  }

  private static String resolveInputSchema(Class<? extends Component> componentClass,
      McpApp annotation, McpAppMethodDescriptor openingInputMethod) {
    boolean declaresDocument = !annotation.inputSchema().isBlank();
    boolean declaresClass = annotation.input() != Void.class;

    if (openingInputMethod != null) {
      if (declaresDocument || declaresClass) {
        throw new IllegalArgumentException("The view " + componentClass.getName()
            + " declares an @McpAppInput method together with input or inputSchema on @McpApp. The"
            + " opening input has one source, declare it in one place.");
      }

      return openingInputMethod.getInputSchema();
    }

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
      return McpAppSchemas.generateSchemaDocument(input);
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

    return trimmed.replace('/', '_').toLowerCase(Locale.ROOT);
  }
}
