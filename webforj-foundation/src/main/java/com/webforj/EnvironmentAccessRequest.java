package com.webforj;

import java.util.function.Supplier;

/**
 * Represents a queued access request for executing code in the Environment's thread. This class is
 * used internally by the Environment to queue and process requests from background threads.
 *
 * @author Hyyan Abo Fakher
 * @since 25.02
 */
class EnvironmentAccessRequest {
  private final String id;
  private final Supplier<Object> supplier;
  private final PendingResult<Object> pendingResult;

  /**
   * Constructs a new access request.
   *
   * @param id the unique identifier for this request
   * @param supplier the supplier to execute in the Environment's thread
   * @param pendingResult the pending result to complete when the supplier is executed
   */
  @SuppressWarnings("unchecked")
  EnvironmentAccessRequest(String id, Supplier<?> supplier, PendingResult<?> pendingResult) {
    this.id = id;
    this.supplier = (Supplier<Object>) supplier;
    this.pendingResult = (PendingResult<Object>) pendingResult;
  }

  /**
   * Gets the unique identifier for this request.
   *
   * @return the request ID
   */
  String getId() {
    return id;
  }

  /**
   * Gets the supplier to execute.
   *
   * @return the supplier
   */
  Supplier<Object> getSupplier() {
    return supplier;
  }

  /**
   * Gets the pending result to complete.
   *
   * @return the pending result
   */
  PendingResult<Object> getPendingResult() {
    return pendingResult;
  }
}
