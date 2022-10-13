package org.dwcj.controls;

public interface IScrollable {
    
    Integer getHorizontalScrollBarHeight();

    Integer getHorizontalScrollBarPosition();

    Integer getHorizontalScrollBarWidth();

    Integer getVerticalScrollBarHeight();

    Integer getVerticalScrollBarPosition();

    Integer getVerticalScrollBarWidth();

    Boolean isHorizontalScrollBarVisible();

    Boolean isVerticalScrollBarVisible();

    IScrollable setHorizontalScrollBarPosition(Integer position);

    IScrollable setVerticalScrollBarPosition(Integer position);
}
