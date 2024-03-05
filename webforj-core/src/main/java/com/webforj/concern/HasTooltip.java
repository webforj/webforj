package com.webforj.concern;

import com.webforj.component.Component;

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
  public String getTooltipText();

  /**
   * Sets the tooltip text of the component.
   *
   * @param tooltipText the tooltip text to set.
   * @return the component itself.
   */
  public T setTooltipText(String tooltipText);
}
