package com.webforj.devtools.craftforj.inspector.contribution.layout.columnslayout.item;

import com.webforj.component.Component;
import com.webforj.concern.HasAttribute;
import com.webforj.concern.HasStyle;

/**
 * Test helper for columns layout item contribution tests.
 */
final class ColumnsLayoutItemTestHelper {

  private ColumnsLayoutItemTestHelper() {}

  /** Mock-compatible component with HasStyle. */
  abstract static class StylableComponent extends Component implements HasStyle<StylableComponent> {
  }

  /** Mock-compatible component with HasAttribute. */
  abstract static class AttributableComponent extends Component
      implements HasAttribute<AttributableComponent> {
  }
}
