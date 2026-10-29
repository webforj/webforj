package com.webforj.matchmedia;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.webforj.Page;
import com.webforj.PendingResult;
import com.webforj.dispatcher.ListenerRegistration;
import com.webforj.environment.ObjectTable;
import com.webforj.event.page.PageEvent;
import com.webforj.event.page.PageEventOptions;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Objects;
import java.util.UUID;
import java.util.function.Consumer;

/**
 * Provides access to the browser's {@code window.matchMedia} API for the current environment.
 *
 * <p>
 * Each {@link #matchMedia(String)} call creates an independently owned query. Browser results are
 * asynchronous; changes are delivered through typed listeners on the resulting query.
 * </p>
 *
 * @since 26.03
 */
public final class MediaQuery {

  private final Map<String, MediaQueryList> queries = new LinkedHashMap<>();
  private MediaQueryBridge bridge;
  private ListenerRegistration<PageEvent> pageRegistration;
  private boolean destroyed;

  private MediaQuery() {}

  /**
   * Returns the media query service for the current environment, creating it when needed.
   *
   * @return the media query service
   */
  public static MediaQuery getCurrent() {
    String key = MediaQuery.class.getName();
    if (ObjectTable.contains(key)) {
      return (MediaQuery) ObjectTable.get(key);
    }
    MediaQuery instance = new MediaQuery();
    ObjectTable.put(key, instance);
    return instance;
  }

  /**
   * Checks whether a service has already been created in the current environment. This does not
   * create a service or check browser support.
   *
   * @return whether an existing service is present
   */
  public static boolean isPresent() {
    return ObjectTable.contains(MediaQuery.class.getName());
  }

  /**
   * Executes a consumer with the existing service, without creating one.
   *
   * @param consumer the consumer to execute
   */
  public static void ifPresent(Consumer<MediaQuery> consumer) {
    Objects.requireNonNull(consumer, "consumer");
    if (isPresent()) {
      consumer.accept(getCurrent());
    }
  }

  /**
   * Creates a browser media query. Query syntax and serialization are handled by the browser.
   *
   * @param query the CSS media query, including an empty string if desired
   * @return a result that resolves once the browser query is initialized, or fails if browser
   *         support or communication is unavailable
   * @throws NullPointerException if the query is null
   * @throws IllegalStateException if this service has been destroyed
   */
  public PendingResult<MediaQueryList> matchMedia(String query) {
    requireActive();
    Objects.requireNonNull(query, "query");
    ensureBridge();
    String id = UUID.randomUUID().toString();
    MediaQueryList list = new MediaQueryList(this, id);
    queries.put(id, list);
    JsonObject request = MediaQueryBridge.request("create", id);
    request.addProperty("query", query);
    PendingResult<MediaQueryList> result = call(request).thenApply(value -> {
      list.initialize(value);
      return list;
    });
    result.exceptionally(error -> {
      list.destroy();
      return null;
    });
    return result;
  }

  /**
   * Disposes all queries and the shared page listener. Repeated calls have no effect. The
   * application lifecycle also calls this before page teardown.
   */
  public void destroy() {
    if (destroyed) {
      return;
    }
    destroyed = true;
    if (isPresent() && ObjectTable.get(MediaQuery.class.getName()) == this) {
      ObjectTable.put(MediaQuery.class.getName(), null);
    }

    Map<String, MediaQueryList> owned = new LinkedHashMap<>(queries);
    queries.clear();
    owned.values().forEach(MediaQueryList::dispose);
    if (bridge == null) {
      return;
    }
    owned.keySet().forEach(bridge::cancel);
    JsonArray ids = new JsonArray();
    owned.keySet().forEach(ids::add);
    JsonObject request = new JsonObject();
    request.addProperty("command", "destroyAll");
    request.add("ids", ids);
    try {
      bridge.send(request);
    } finally {
      pageRegistration.remove();
    }
  }

  PendingResult<JsonElement> call(JsonObject request) {
    requireActive();
    return bridge.call(request);
  }

  void send(JsonObject request) {
    bridge.send(request);
  }

  void release(String id) {
    queries.remove(id);
    bridge.cancel(id);
    bridge.send(MediaQueryBridge.request("destroy", id));
  }

  private void ensureBridge() {
    if (bridge != null) {
      return;
    }
    Page page = Page.getCurrent();
    PageEventOptions options = new PageEventOptions();
    options.addData("payload", "JSON.stringify(event.detail)");
    pageRegistration =
        page.addEventListener(MediaQueryBridge.EVENT_NAME, this::handleEvent, options);
    bridge = new MediaQueryBridge(page);
  }

  private void handleEvent(PageEvent event) {
    if (destroyed) {
      return;
    }
    Object payload = event.getData().get("payload");
    if (!(payload instanceof String json)) {
      return;
    }
    JsonElement parsed = JsonParser.parseString(json);
    if (!parsed.isJsonObject()) {
      return;
    }
    JsonObject data = parsed.getAsJsonObject();
    MediaQueryList query = queries.get(MediaQueryBridge.stringValue(data, "id"));
    if (query != null) {
      query.dispatch(data);
    }
  }

  private void requireActive() {
    if (destroyed) {
      throw new IllegalStateException("Media query service has been destroyed");
    }
  }
}
