package com.webforj.concern.legacy;


/**
 * On applicable controls, helps facilitate tooltip functionality to facilitate implementation of
 * methods to interact with this behavior.
 *
 * @deprecated Use {@link HasTooltip} instead.
 */
@Deprecated(since = "23.05", forRemoval = true)
public interface LegacyHasTooltip {

  /**
   * get the tooltip text property of the control.
   *
   * @return the tooltip text
   */
  public String getTooltipText();

  /**
   * Set the tooltip text of the control.
   *
   * @param tooltipText the tooltip text to set
   * @return the control itself
   */
  public LegacyHasTooltip setTooltipText(String tooltipText);

}
