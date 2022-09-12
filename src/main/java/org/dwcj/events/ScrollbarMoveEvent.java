package org.dwcj.events;

import org.dwcj.controls.ScrollBar;

public class ScrollbarMoveEvent implements IDwcEvent {

    private final ScrollBar control;

    public ScrollbarMoveEvent(ScrollBar scrollBar) { this.control = scrollBar; }

    @Override
    public ScrollBar getControl() { return control; }
}
