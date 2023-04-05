package org.dwcj.component;

/**
 * Interface which facilitates implementation of methods to handle controls with scrolling behavior.
 */
public interface Scrollable {

  /**
   * This method returns the height of the horizontal scrollbar for a Scrollable Control.
   *
   * @return Integer height of scrollbar
   */
  Integer getHorizontalScrollBarHeight();

  /**
   * This method returns the position of the horizontal scrollbar for a Scrollable Control.
   *
   * @return Integer horizontal position of scrollbar
   */
  Integer getHorizontalScrollBarPosition();

  /**
   * This method returns the width of the horizontal scrollbar for a Scrollable Control.
   *
   * @return Integer horizontal scrollbar width
   */
  Integer getHorizontalScrollBarWidth();

  /**
   * This method returns the height of the vertical scrollbar for a Scrollable Control.
   *
   * @return Integer vertical scrollbar height
   */
  Integer getVerticalScrollBarHeight();

  /**
   * This method returns the position of the vertical scrollbar for a Scrollable Control.
   *
   * @return Integer representing vertical scroll bar's position
   */
  Integer getVerticalScrollBarPosition();

  /**
   * This method returns the width of the vertical scrollbar for a Scrollable Control.
   *
   * @return Integer representing width of the vertical scrollbar
   */
  Integer getVerticalScrollBarWidth();

  /**
   * This method returns whether the horizontal scrollbar for a Scrollable Control is currently
   * visible.
   *
   * @return True if horizontal bar is visible, False if not.
   */
  Boolean isHorizontalScrollBarVisible();

  /**
   * This method returns whether the vertical scrollbar for a Scrollable Control is currently
   * visible.
   *
   * @return True if vertical bar is visible, False if not.
   */
  Boolean isVerticalScrollBarVisible();

  /**
   * This method sets the position of the horizontal scrollbar for a Scrollable BBjControl.
   *
   * @param position Integer pixel location of beginning of horizontal scrollbar
   * @return The control itself
   */
  Scrollable setHorizontalScrollBarPosition(Integer position);

  /**
   * This method sets the position of the vertical scrollbar for a Scrollable BBjControl.
   *
   * @param position Integer pixel location of beginning of vertical scrollbar
   * @return The control itself
   */
  Scrollable setVerticalScrollBarPosition(Integer position);
}
