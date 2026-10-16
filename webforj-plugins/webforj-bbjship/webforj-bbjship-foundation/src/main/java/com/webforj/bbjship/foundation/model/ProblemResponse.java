package com.webforj.bbjship.foundation.model;

/**
 * Holds the problem details BBjShip answers to a refused request, as RFC 9457 defines them.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class ProblemResponse {

  private String detail;

  /**
   * Gets the explanation of this occurrence of the problem.
   *
   * @return the detail, null when the answer carries none
   */
  public String getDetail() {
    return detail;
  }
}
