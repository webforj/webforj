package com.webforj.mcp;

import com.webforj.component.Component;
import com.webforj.mcp.annotation.McpAppAction;
import java.lang.reflect.Method;
import java.util.Locale;

/**
 * Describes the tool projected by one action method.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
final class McpAppActionDescriptor extends McpAppMethodDescriptor {

  private final String nameSegment;
  private final String toolName;
  private final String description;

  McpAppActionDescriptor(String appToolName, Class<? extends Component> viewType,
      Method invocationMethod, Class<?> ownerType) {
    super(viewType, invocationMethod, ownerType);

    McpAppAction annotation = invocationMethod.getAnnotation(McpAppAction.class);
    if (annotation.description().isBlank()) {
      throw new IllegalArgumentException("@McpAppAction requires a description on method "
          + describeMethod(invocationMethod) + ".");
    }

    this.nameSegment =
        annotation.name().isBlank() ? toNameSegment(invocationMethod.getName()) : annotation.name();
    this.toolName = appToolName + "_" + nameSegment;
    this.description = annotation.description();
  }

  String getNameSegment() {
    return nameSegment;
  }

  String getToolName() {
    return toolName;
  }

  String getDescription() {
    return description;
  }

  private static String toNameSegment(String methodName) {
    return methodName.replaceAll("([a-z0-9])([A-Z])", "$1_$2").toLowerCase(Locale.ROOT);
  }
}
