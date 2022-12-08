package org.dwcj.controls.scrollbar.events;

import org.dwcj.controls.scrollbar.ScrollBar;
import org.dwcj.interfaces.IDwcEvent;

public class ScrollbarMoveEvent implements IDwcEvent {

    private final ScrollBar control;

    public ScrollbarMoveEvent(ScrollBar scrollBar) { this.control = scrollBar; }

    @Override
    public ScrollBar getControl() { return control; }
}
