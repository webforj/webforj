package com.webforj.devtools.craftforj.inspector.contribution.layout.flexlayout.item;

import com.webforj.component.Component;
import com.webforj.concern.HasStyle;

/**
 * Test helper for flex item contribution tests.
 */
final class FlexItemTestHelper {

  private FlexItemTestHelper() {}

  /** Mock-compatible component with HasStyle. */
  abstract static class StylableComponent extends Component implements HasStyle<StylableComponent> {
  }
}
