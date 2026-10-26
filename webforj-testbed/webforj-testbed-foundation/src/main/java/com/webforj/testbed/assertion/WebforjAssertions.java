package com.webforj.testbed.assertion;

import com.webforj.component.Component;

/**
 * The entry point to the assertions on webforJ components.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class WebforjAssertions {

  private WebforjAssertions() {}

  /**
   * Opens assertions on the given component.
   *
   * @param actual the component under test
   * @return the assertion
   */
  public static ComponentAssert verifyThat(Component actual) {
    return new ComponentAssert(actual);
  }
}
