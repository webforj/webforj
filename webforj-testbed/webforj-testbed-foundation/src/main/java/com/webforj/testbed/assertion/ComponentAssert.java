package com.webforj.testbed.assertion;

import com.webforj.component.Component;

/**
 * Assertions on any webforJ component.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class ComponentAssert extends AbstractComponentAssert<ComponentAssert, Component> {

  /**
   * Creates a new assertion.
   *
   * @param actual the component under test
   */
  ComponentAssert(Component actual) {
    super(actual, ComponentAssert.class);
  }
}
