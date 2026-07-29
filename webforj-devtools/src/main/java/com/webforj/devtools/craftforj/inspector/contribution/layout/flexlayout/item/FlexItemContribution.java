package com.webforj.devtools.craftforj.inspector.contribution.layout.flexlayout.item;

import com.webforj.component.Component;
import com.webforj.component.layout.flexlayout.FlexLayout;
import com.webforj.concern.HasStyle;
import com.webforj.devtools.craftforj.inspector.contribution.LayoutItemContribution;
import com.webforj.devtools.craftforj.inspector.source.generator.SourceChange;

/**
 * Base contribution for flex item properties.
 *
 * <p>
 * Flex item properties are applied through the parent {@link FlexLayout}'s item API, e.g.
 * {@code flexLayout.setItemGrow(1, item)}, both for live changes and generated source code. The
 * current value is read from the child's style because that is where FlexLayout stores item
 * settings.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public abstract class FlexItemContribution extends LayoutItemContribution<FlexLayout> {

  private final String cssKey;

  /**
   * Creates a flex item contribution.
   *
   * @param cssKey the CSS property key FlexLayout stores the setting under (e.g., "flex-grow")
   * @param displayName the display name shown in UI (e.g., "Grow")
   * @param methodName the FlexLayout item API method (e.g., "setItemGrow")
   */
  protected FlexItemContribution(String cssKey, String displayName, String methodName) {
    super(FlexLayout.class, HasStyle.class, displayName, methodName,
        SourceChange.ItemPosition.LAST);
    this.cssKey = cssKey;
  }

  /**
   * Reads the raw style value FlexLayout stores on the child for this property.
   *
   * @param component the child component
   * @return the style value, or null when unset
   */
  protected String readStyle(Component component) {
    String value = ((HasStyle<?>) component).getStyle(cssKey);
    return (value == null || value.isEmpty()) ? null : value;
  }

  /**
   * Clears the style FlexLayout stores on the child for this property.
   *
   * <p>
   * Used as the live resetter for properties whose FlexLayout API has no clearing form; removing
   * the style is the exact inverse of what the API writes.
   * </p>
   *
   * @param component the child component
   */
  protected void clearStyle(Component component) {
    ((HasStyle<?>) component).setStyle(cssKey, "");
  }
}
