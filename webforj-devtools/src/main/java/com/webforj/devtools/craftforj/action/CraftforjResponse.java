package com.webforj.devtools.craftforj.action;

/**
 * Represents a response sent back to the craftforJ extension.
 *
 * <p>
 * Responses are JSON objects with the structure:
 * </p>
 *
 * <pre>
 * {
 *   "requestId": "unique-id",
 *   "success": true|false,
 *   "data": { ... } | null,
 *   "error": "error message" | null
 * }
 * </pre>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
class CraftforjResponse {

  private final String requestId;
  private final boolean success;
  private final Object data;
  private final String error;

  private CraftforjResponse(String requestId, boolean success, Object data, String error) {
    this.requestId = requestId;
    this.success = success;
    this.data = data;
    this.error = error;
  }

  /**
   * Creates a successful response with data.
   *
   * @param requestId the request ID
   * @param data the response data
   * @return the response
   */
  static CraftforjResponse success(String requestId, Object data) {
    return new CraftforjResponse(requestId, true, data, null);
  }

  /**
   * Creates an error response.
   *
   * @param requestId the request ID
   * @param error the error message
   * @return the response
   */
  static CraftforjResponse error(String requestId, String error) {
    return new CraftforjResponse(requestId, false, null, error);
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
   * Returns whether the request was successful.
   *
   * @return true if successful
   */
  boolean isSuccess() {
    return success;
  }

  /**
   * Gets the response data.
   *
   * @return the data, or null if error
   */
  Object getData() {
    return data;
  }

  /**
   * Gets the error message.
   *
   * @return the error, or null if successful
   */
  String getError() {
    return error;
  }
}
