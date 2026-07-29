package com.webforj.devtools.craftforj.action;

import com.google.gson.Gson;
import com.google.gson.JsonObject;
import com.google.gson.JsonSyntaxException;

/**
 * Represents a request from the craftforJ extension.
 *
 * <p>
 * Requests are JSON objects with the structure:
 * </p>
 *
 * <pre>
 * {
 *   "requestId": "unique-id",
 *   "action": "actionName",
 *   "nonce": "channel-nonce",
 *   "params": { ... }
 * }
 * </pre>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
class CraftforjRequest {

  private static final Gson GSON = new Gson();

  private String requestId;
  private String action;
  private String nonce;
  private JsonObject params;

  /**
   * Parses a craftforJ request from JSON.
   *
   * @param json the JSON string to parse
   * @return the parsed request, or {@code null} when the JSON is missing or malformed
   */
  static CraftforjRequest fromJson(String json) {
    if (json == null || json.isEmpty()) {
      return null;
    }

    CraftforjRequest request;
    try {
      request = GSON.fromJson(json, CraftforjRequest.class);
    } catch (JsonSyntaxException e) {
      return null;
    }

    if (request == null) {
      return null;
    }
    if (request.params == null) {
      request.params = new JsonObject();
    }

    return request;
  }

  /**
   * Gets the request ID.
   *
   * @return the request ID
   */
  String getRequestId() {
    return requestId;
  }

  /**
   * Gets the action name.
   *
   * @return the action name
   */
  String getAction() {
    return action;
  }

  /**
   * Gets the channel nonce carried by the request.
   *
   * @return the nonce, or {@code null} when the request carries none
   */
  String getNonce() {
    return nonce;
  }

  /**
   * Gets the request parameters.
   *
   * @return the parameters as JsonObject
   */
  JsonObject getParams() {
    return params;
  }
}
