package com.webforj.mcp.event;

import java.util.EventObject;
import tools.jackson.databind.JsonNode;
import tools.jackson.databind.json.JsonMapper;

/**
 * Carries arguments sent to a running view through its update tool.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class McpAppUpdateEvent extends EventObject {

  private final String toolName;
  private final transient JsonNode arguments;

  /**
   * Creates the event.
   *
   * @param source the host connection the event arrived on
   * @param toolName the name of the called tool
   * @param arguments the call arguments
   */
  public McpAppUpdateEvent(Object source, String toolName, JsonNode arguments) {
    super(source);
    this.toolName = toolName;
    this.arguments = arguments;
  }

  /**
   * Returns the name of the tool that sent the update.
   *
   * @return the tool name
   */
  public String getToolName() {
    return toolName;
  }

  /**
   * Returns the call arguments.
   *
   * @return the arguments
   */
  public JsonNode getArguments() {
    return arguments;
  }

  /**
   * Returns the call arguments bound to the given type.
   *
   * <p>
   * The conversion uses the shared Jackson mapper.
   * </p>
   *
   * @param <T> the type to bind to
   * @param type the class of the type
   * @return the bound arguments
   */
  public <T> T getArgumentsAs(Class<T> type) {
    return JsonMapper.shared().convertValue(arguments, type);
  }
}
