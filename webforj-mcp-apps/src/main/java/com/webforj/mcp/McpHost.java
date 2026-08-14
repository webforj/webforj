package com.webforj.mcp;

import com.webforj.Page;
import com.webforj.PendingResult;
import com.webforj.dispatcher.EventDispatcher;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;
import com.webforj.environment.ObjectTable;
import com.webforj.mcp.event.McpHostContextChangedEvent;
import com.webforj.mcp.event.McpToolCancelledEvent;
import com.webforj.mcp.event.McpToolInputEvent;
import com.webforj.mcp.event.McpToolInputPartialEvent;
import com.webforj.mcp.event.McpToolResultEvent;
import com.webforj.router.Router;
import com.webforj.router.history.Location;
import io.modelcontextprotocol.spec.McpSchema.CallToolResult;
import io.modelcontextprotocol.spec.McpSchema.Implementation;
import io.modelcontextprotocol.spec.McpSchema.ListResourcesResult;
import io.modelcontextprotocol.spec.McpSchema.ReadResourceResult;
import java.lang.System.Logger;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;
import tools.jackson.databind.JsonNode;
import tools.jackson.databind.json.JsonMapper;
import tools.jackson.databind.node.ObjectNode;

/**
 * The MCP host an embedded application talks to.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public final class McpHost {

  static final String OBJECT_TABLE_KEY = McpHost.class.getName();

  private static final Logger logger = System.getLogger(McpHost.class.getName());
  private static final JsonMapper mapper = JsonMapper.shared();
  private static final String ROUTE_FIELD = "_route";
  private static final String ARGUMENTS_FIELD = "arguments";

  private final Page page;
  private final EventDispatcher dispatcher = new EventDispatcher();
  private final Map<String, PendingResult<JsonNode>> pendingResults = new ConcurrentHashMap<>();
  private final AtomicLong callIds = new AtomicLong();
  private final AtomicReference<JsonNode> hostInfo = new AtomicReference<>();
  private final AtomicReference<JsonNode> hostCapabilities = new AtomicReference<>();
  private final AtomicReference<ObjectNode> hostContext = new AtomicReference<>();
  private final AtomicBoolean openingToolResult = new AtomicBoolean(true);

  McpHost(Page page) {
    this.page = page;
    ObjectTable.put(McpHost.OBJECT_TABLE_KEY, this);
  }

  /**
   * Gets the current host connection instance.
   *
   * @return the current host connection instance, {@code null} when the application does not run
   *         under an MCP host
   */
  public static McpHost getCurrent() {
    if (ObjectTable.contains(OBJECT_TABLE_KEY)
        && ObjectTable.get(OBJECT_TABLE_KEY) instanceof McpHost host) {
      return host;
    }

    return null;
  }

  /**
   * Checks if the host connection is present.
   *
   * @return true if the host connection is present
   */
  public static boolean isPresent() {
    return getCurrent() != null;
  }

  /**
   * Executes the given consumer when the host connection is present.
   *
   * @param consumer the consumer to execute
   */
  public static void ifPresent(Consumer<McpHost> consumer) {
    if (isPresent()) {
      consumer.accept(getCurrent());
    }
  }

  /**
   * Calls a tool of the server through the host.
   *
   * @param name the tool name
   * @param arguments the tool arguments
   * @return the pending call result
   */
  public PendingResult<CallToolResult> callTool(String name, Map<String, Object> arguments) {
    return request("tools/call",
        Map.of("name", name, ARGUMENTS_FIELD, arguments == null ? Map.of() : arguments))
        .thenApply(answer -> mapper.convertValue(answer, CallToolResult.class));
  }

  /**
   * Reads a resource of the server through the host.
   *
   * @param uri the resource uri
   * @return the pending read result
   */
  public PendingResult<ReadResourceResult> readResource(String uri) {
    return request("resources/read", Map.of("uri", uri))
        .thenApply(answer -> mapper.convertValue(answer, ReadResourceResult.class));
  }

  /**
   * Lists the resources of the server through the host.
   *
   * @return the pending listing result
   */
  public PendingResult<ListResourcesResult> listResources() {
    return request("resources/list", Map.of())
        .thenApply(answer -> mapper.convertValue(answer, ListResourcesResult.class));
  }

  /**
   * Sends a user message into the conversation.
   *
   * @param text the message text
   * @return the pending result
   */
  public PendingResult<Void> sendMessage(String text) {
    return request("ui/message",
        Map.of("role", "user", "content", List.of(Map.of("type", "text", "text", text))))
        .thenApply(answer -> null);
  }

  /**
   * Replaces the model context the application contributes with the given text.
   *
   * @param content the context text
   * @return the pending result
   */
  public PendingResult<Void> updateModelContext(String content) {
    return request("ui/update-model-context",
        Map.of("content", List.of(Map.of("type", "text", "text", content))))
        .thenApply(answer -> null);
  }

  /**
   * Replaces the model context the application contributes with the given structured data.
   *
   * @param structuredContent the structured context
   * @return the pending result
   */
  public PendingResult<Void> updateModelContext(Map<String, Object> structuredContent) {
    return request("ui/update-model-context", Map.of("structuredContent", structuredContent))
        .thenApply(answer -> null);
  }

  /**
   * Asks the host to open a link outside the application frame.
   *
   * @param url the address to open
   * @return the pending result
   */
  public PendingResult<Void> openLink(String url) {
    return request("ui/open-link", Map.of("url", url)).thenApply(answer -> null);
  }

  /**
   * Asks the host for a display mode.
   *
   * @param mode the display mode
   * @return the pending result, carrying the mode the host settled on
   * @throws IllegalArgumentException if no mode is given
   */
  public PendingResult<McpAppDisplayMode> requestDisplayMode(McpAppDisplayMode mode) {
    if (mode == null) {
      throw new IllegalArgumentException("Name the display mode to request.");
    }

    return request("ui/request-display-mode", Map.of("mode", mode.getValue()))
        .thenApply(answer -> McpAppDisplayMode.fromValue(answer.path("mode").asString()));
  }

  /**
   * Returns the identity the host reported in the handshake.
   *
   * @return the host info, empty before the handshake answer arrived
   */
  public Optional<Implementation> getHostInfo() {
    return Optional.ofNullable(hostInfo.get())
        .map(node -> mapper.convertValue(node, Implementation.class));
  }

  /**
   * Returns the capabilities the host reported in the handshake.
   *
   * @return the host capabilities, empty before the handshake answer arrived
   */
  public Optional<McpHostCapabilities> getHostCapabilities() {
    return Optional.ofNullable(hostCapabilities.get())
        .map(node -> mapper.convertValue(node, McpHostCapabilities.class));
  }

  /**
   * Returns the context the host reported, kept current with context change notifications.
   *
   * @return the host context, empty while the host reported none
   */
  public Optional<McpHostContext> getHostContext() {
    return Optional.ofNullable(hostContext.get())
        .map(node -> mapper.convertValue(node, McpHostContext.class));
  }

  /**
   * Adds a listener for the complete tool arguments the host delivers.
   *
   * @param listener the listener to add
   * @return a listener registration for removing the event listener
   */
  public ListenerRegistration<McpToolInputEvent> addToolInputListener(
      EventListener<McpToolInputEvent> listener) {
    return dispatcher.addListener(McpToolInputEvent.class, listener);
  }

  /**
   * Alias for {@link #addToolInputListener(EventListener)}.
   *
   * @param listener the listener to add
   * @return a listener registration for removing the event listener
   */
  public ListenerRegistration<McpToolInputEvent> onToolInput(
      EventListener<McpToolInputEvent> listener) {
    return addToolInputListener(listener);
  }

  /**
   * Adds a listener for the partial tool arguments the host streams.
   *
   * @param listener the listener to add
   * @return a listener registration for removing the event listener
   */
  public ListenerRegistration<McpToolInputPartialEvent> addToolInputPartialListener(
      EventListener<McpToolInputPartialEvent> listener) {
    return dispatcher.addListener(McpToolInputPartialEvent.class, listener);
  }

  /**
   * Alias for {@link #addToolInputPartialListener(EventListener)}.
   *
   * @param listener the listener to add
   * @return a listener registration for removing the event listener
   */
  public ListenerRegistration<McpToolInputPartialEvent> onToolInputPartial(
      EventListener<McpToolInputPartialEvent> listener) {
    return addToolInputPartialListener(listener);
  }

  /**
   * Adds a listener for the finished tool result the host delivers.
   *
   * @param listener the listener to add
   * @return a listener registration for removing the event listener
   */
  public ListenerRegistration<McpToolResultEvent> addToolResultListener(
      EventListener<McpToolResultEvent> listener) {
    return dispatcher.addListener(McpToolResultEvent.class, listener);
  }

  /**
   * Alias for {@link #addToolResultListener(EventListener)}.
   *
   * @param listener the listener to add
   * @return a listener registration for removing the event listener
   */
  public ListenerRegistration<McpToolResultEvent> onToolResult(
      EventListener<McpToolResultEvent> listener) {
    return addToolResultListener(listener);
  }

  /**
   * Adds a listener for the host cancelling the running tool call.
   *
   * @param listener the listener to add
   * @return a listener registration for removing the event listener
   */
  public ListenerRegistration<McpToolCancelledEvent> addToolCancelledListener(
      EventListener<McpToolCancelledEvent> listener) {
    return dispatcher.addListener(McpToolCancelledEvent.class, listener);
  }

  /**
   * Alias for {@link #addToolCancelledListener(EventListener)}.
   *
   * @param listener the listener to add
   * @return a listener registration for removing the event listener
   */
  public ListenerRegistration<McpToolCancelledEvent> onToolCancelled(
      EventListener<McpToolCancelledEvent> listener) {
    return addToolCancelledListener(listener);
  }

  /**
   * Adds a listener for host context changes.
   *
   * @param listener the listener to add
   * @return a listener registration for removing the event listener
   */
  public ListenerRegistration<McpHostContextChangedEvent> addHostContextChangedListener(
      EventListener<McpHostContextChangedEvent> listener) {
    return dispatcher.addListener(McpHostContextChangedEvent.class, listener);
  }

  /**
   * Alias for {@link #addHostContextChangedListener(EventListener)}.
   *
   * @param listener the listener to add
   * @return a listener registration for removing the event listener
   */
  public ListenerRegistration<McpHostContextChangedEvent> onHostContextChanged(
      EventListener<McpHostContextChangedEvent> listener) {
    return addHostContextChangedListener(listener);
  }

  void dispatch(String json) {
    JsonNode message;
    try {
      message = mapper.readTree(json);
    } catch (RuntimeException e) {
      logger.log(Logger.Level.WARNING, "Discarding an unreadable host message", e);
      return;
    }

    if (message == null || !message.isObject()) {
      logger.log(Logger.Level.WARNING, "Discarding an unreadable host message");
      return;
    }

    String type = message.path("type").asString("");
    JsonNode payload =
        message.path("payload").isObject() ? message.path("payload") : mapper.createObjectNode();

    switch (type) {
      case "initialized" -> initialized(payload);
      case "response" -> complete(message);
      case "tool-input" ->
        dispatcher.dispatchEvent(new McpToolInputEvent(this, arguments(payload)));
      case "tool-input-partial" ->
        dispatcher.dispatchEvent(new McpToolInputPartialEvent(this, arguments(payload)));
      case "tool-result" -> toolResult(payload);
      case "tool-cancelled" -> dispatcher.dispatchEvent(new McpToolCancelledEvent(this,
          payload.path("reason").isString() ? payload.path("reason").asString() : null));
      case "host-context-changed" -> hostContextChanged(payload);
      case "teardown" -> logger.log(Logger.Level.DEBUG, "The host is tearing the application down");
      default -> logger.log(Logger.Level.DEBUG, () -> "Unknown host message type: " + type);
    }
  }

  void ready() {
    page.executeJsVoidAsync("window.__webforjMcpChannel && window.__webforjMcpChannel.ready()");
  }

  void destroy() {
    ObjectTable.put(OBJECT_TABLE_KEY, null);
    dispatcher.removeAllListeners();
    pendingResults.values().forEach(pending -> pending.completeExceptionally(
        new IllegalStateException("The application terminated before the host answered")));
    pendingResults.clear();
  }

  private PendingResult<JsonNode> request(String method, Map<String, Object> params) {
    String callId = "call-" + callIds.incrementAndGet();
    PendingResult<JsonNode> result = new PendingResult<>();
    pendingResults.put(callId, result);

    page.executeJsVoidAsync("window.__webforjMcpChannel && window.__webforjMcpChannel.request("
        + mapper.writeValueAsString(callId) + "," + mapper.writeValueAsString(method) + ","
        + mapper.writeValueAsString(params) + ")");

    return result;
  }

  private JsonNode arguments(JsonNode payload) {
    return payload.path(ARGUMENTS_FIELD).isObject() ? payload.path(ARGUMENTS_FIELD)
        : mapper.createObjectNode();
  }

  private void initialized(JsonNode payload) {
    if (payload.path("hostInfo").isObject()) {
      hostInfo.set(payload.path("hostInfo"));
    }
    if (payload.path("hostCapabilities").isObject()) {
      hostCapabilities.set(payload.path("hostCapabilities"));
    }
    if (payload.path("hostContext").isObject()) {
      hostContext.set((ObjectNode) payload.path("hostContext").deepCopy());
    }
  }

  private void complete(JsonNode message) {
    String callId = message.path("callId").isString() ? message.path("callId").asString() : null;
    PendingResult<JsonNode> pending = callId == null ? null : pendingResults.remove(callId);
    if (pending == null) {
      return;
    }

    JsonNode error = message.path("error");
    if (!error.isMissingNode() && !error.isNull()) {
      pending.completeExceptionally(
          new IllegalStateException("The host answered with an error: " + error));
      return;
    }

    JsonNode result = message.path("result");
    pending.complete(result.isObject() ? result : mapper.createObjectNode());
  }

  private void toolResult(JsonNode payload) {
    // The first result always answers the call that opened the frame, whose route the frame
    // already booted at and the router already resolved, security redirects included.
    // Re-asserting that route would tear the settled view down, so it never navigates.
    if (!openingToolResult.getAndSet(false)) {
      navigate(payload);
    }

    CallToolResult result = mapper.convertValue(payload, CallToolResult.class);
    dispatcher.dispatchEvent(new McpToolResultEvent(this, result));
  }

  private void navigate(JsonNode payload) {
    JsonNode route = payload.path("structuredContent").path(ROUTE_FIELD);
    if (!route.isString()) {
      return;
    }

    Router router = Router.getCurrent();
    if (router == null) {
      // A deployment without the router published no view tools, so a route cannot arrive here
      // in practice, and without a router there is nothing to navigate.
      return;
    }

    router.navigate(new Location(route.asString()));
  }

  private void hostContextChanged(JsonNode payload) {
    ObjectNode current = hostContext.get();
    if (current == null) {
      hostContext.set((ObjectNode) payload.deepCopy());
    } else if (payload.isObject()) {
      current.setAll((ObjectNode) payload);
    }

    dispatcher.dispatchEvent(new McpHostContextChangedEvent(this, payload));
  }
}
