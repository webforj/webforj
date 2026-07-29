package com.webforj.devtools.craftforj.keys.action;

import com.google.gson.JsonObject;
import com.webforj.devtools.craftforj.action.CraftforjActionException;

/**
 * Shared request parameter extraction for the keys actions.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
final class KeysParams {

  private KeysParams() {}

  /**
   * Reads a required non-blank string parameter.
   *
   * @param params the request params
   * @param name the parameter name
   * @return the parameter value
   * @throws CraftforjActionException when the parameter is missing or blank
   */
  static String requireString(JsonObject params, String name) {
    if (params != null && params.has(name) && !params.get(name).isJsonNull()) {
      String value = params.get(name).getAsString();
      if (value != null && !value.isBlank()) {
        return value;
      }
    }

    throw new CraftforjActionException("Missing required parameter: " + name);
  }
}
