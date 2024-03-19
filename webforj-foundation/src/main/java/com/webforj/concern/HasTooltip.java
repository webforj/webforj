package com.webforj.concern;

import com.webforj.component.Component;
import com.webforj.component.ComponentUtil;

/**
 * An interface for implementing methods to set and retrieve tooltip text on a component.
 *
 * @param <T> the type of the component that implements this interface.
 *
 * @author Hyyan Abo Fakher
 * @since 23.05
 */
public interface HasTooltip<T extends Component> {

  /**
   * Retrieves the tooltip text property of the component.
   *
   * @return the tooltip text of the component.
   */
  public default String getTooltipText() {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof HasTooltip) {
      return ((HasTooltip<?>) component).getTooltipText();
    }

    throw new UnsupportedOperationException(
        "The component does not support the tooltip text property");
  }

  /**
   * Sets the tooltip text of the component.
   *
   * @param tooltipText the tooltip text to set.
   * @return the component itself.
   */
  public default T setTooltipText(String tooltipText) {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof HasTooltip) {
      ((HasTooltip<?>) component).setTooltipText(tooltipText);
      return (T) this;
    }

    throw new UnsupportedOperationException(
        "The component does not support the tooltip text property");
  }
}
