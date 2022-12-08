package org.dwcj.controls.scrollBar.events;

import org.dwcj.controls.scrollBar.ScrollBar;
import org.dwcj.interfaces.IDwcEvent;

public class ScrollbarMoveEvent implements IDwcEvent {

    private final ScrollBar control;

    public ScrollbarMoveEvent(ScrollBar scrollBar) { this.control = scrollBar; }

    @Override
    public ScrollBar getControl() { return control; }
}
