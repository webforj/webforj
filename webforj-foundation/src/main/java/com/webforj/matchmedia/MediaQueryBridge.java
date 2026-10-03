package com.webforj.matchmedia;

import com.google.gson.Gson;
import com.google.gson.JsonElement;
import com.google.gson.JsonNull;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.google.gson.JsonSyntaxException;
import com.webforj.Page;
import com.webforj.PendingResult;
import com.webforj.environment.ObjectTable;
import com.webforj.exceptions.WebforjRuntimeException;
import com.webforj.utilities.Assets;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/** Browser transport shared by the media queries in one environment. */
final class MediaQueryBridge {

  static final String EVENT_NAME = "webforj-media-query-change";
  static final String ASSET_KEY = "webforj.matchmedia.asset-loaded";
  private static final String SCRIPT_PATH = "static/webforj/matchmedia/matchmedia.js";
  private static final Gson GSON = new Gson();

  private final Page page;
  private final Map<String, Set<PendingResult<JsonElement>>> pending = new HashMap<>();

  MediaQueryBridge(Page page) {
    this.page = page;
  }

  PendingResult<JsonElement> call(JsonObject request) {
    String id = stringValue(request, "id");
    PendingResult<JsonElement> result = new PendingResult<>();
    pending.computeIfAbsent(id, key -> new HashSet<>()).add(result);
    try {
      PendingResult<Object> response = page.executeJsAsync(script(request));
      ObjectTable.put(ASSET_KEY, true);
      response.thenAccept(value -> {
        forget(id, result);
        result.complete(toValue(value));
      }).exceptionally(error -> {
        forget(id, result);
        result.completeExceptionally(error);
        return null;
      });
    } catch (RuntimeException error) {
      forget(id, result);
      result.completeExceptionally(error);
    }
    return result;
  }

  void send(JsonObject request) {
    page.executeJsVoidAsync(script(request));
    ObjectTable.put(ASSET_KEY, true);
  }

  void cancel(String id) {
    Set<PendingResult<JsonElement>> results = pending.remove(id);
    if (results != null) {
      for (PendingResult<JsonElement> result : results) {
        result.completeExceptionally(new IllegalStateException("Media query has been destroyed"));
      }
    }
  }

  static JsonObject request(String command, String id) {
    JsonObject request = new JsonObject();
    request.addProperty("command", command);
    request.addProperty("id", id);
    return request;
  }

  static String stringValue(JsonObject object, String name) {
    JsonElement value = object.get(name);
    if (value == null || !value.isJsonPrimitive() || !value.getAsJsonPrimitive().isString()) {
      throw new WebforjRuntimeException("Invalid media query response field: " + name);
    }
    return value.getAsString();
  }

  static boolean booleanValue(JsonObject object, String name) {
    JsonElement value = object.get(name);
    if (value == null || !value.isJsonPrimitive() || !value.getAsJsonPrimitive().isBoolean()) {
      throw new WebforjRuntimeException("Invalid media query response field: " + name);
    }
    return value.getAsBoolean();
  }

  private String script(JsonObject request) {
    String call = "window.__webforjMatchMedia.call(" + GSON.toJson(request) + ")";
    // Install the bridge alongside the first command to avoid an asynchronous asset-loading race.
    return ObjectTable.contains(ASSET_KEY) ? call : Assets.contentOf(SCRIPT_PATH) + "\n" + call;
  }

  private void forget(String id, PendingResult<JsonElement> result) {
    Set<PendingResult<JsonElement>> results = pending.get(id);
    if (results != null) {
      results.remove(result);
      if (results.isEmpty()) {
        pending.remove(id);
      }
    }
  }

  private static JsonElement toValue(Object response) {
    JsonElement parsed;
    try {
      parsed = JsonParser.parseString(String.valueOf(response));
    } catch (JsonSyntaxException error) {
      throw new WebforjRuntimeException("The browser returned an invalid media query response",
          error);
    }
    if (!parsed.isJsonObject()) {
      throw new WebforjRuntimeException("The browser returned no media query response");
    }
    JsonObject envelope = parsed.getAsJsonObject();
    if (!booleanValue(envelope, "ok")) {
      throw new WebforjRuntimeException(stringValue(envelope, "message"));
    }
    return envelope.has("value") ? envelope.get("value") : JsonNull.INSTANCE;
  }
}
