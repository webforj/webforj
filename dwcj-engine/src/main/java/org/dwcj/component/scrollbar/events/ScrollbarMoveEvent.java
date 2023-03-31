package org.dwcj.component.scrollbar.events;

import org.dwcj.component.scrollbar.ScrollBar;
import org.dwcj.interfaces.ControlEvent;

public class ScrollbarMoveEvent implements ControlEvent {

    private final ScrollBar control;

    public ScrollbarMoveEvent(ScrollBar scrollBar) { this.control = scrollBar; }

    @Override
    public ScrollBar getControl() { return control; }
}
