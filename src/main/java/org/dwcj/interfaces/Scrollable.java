package org.dwcj.interfaces;

/**
 * Interface which facilitates implementation of methods to
 * handle controls with scrolling behavior.
 */
public interface Scrollable {
    
    Integer getHorizontalScrollBarHeight();

    Integer getHorizontalScrollBarPosition();

    Integer getHorizontalScrollBarWidth();

    Integer getVerticalScrollBarHeight();

    Integer getVerticalScrollBarPosition();

    Integer getVerticalScrollBarWidth();

    Boolean isHorizontalScrollBarVisible();

    Boolean isVerticalScrollBarVisible();

    Scrollable setHorizontalScrollBarPosition(Integer position);

    Scrollable setVerticalScrollBarPosition(Integer position);
}
