package org.dwcj.controls.tabcontrol.events;

import org.dwcj.controls.tabcontrol.TabControl;
import org.dwcj.interfaces.ControlEvent;

public final class TabSelectEvent implements ControlEvent {
    private final TabControl control;
    private final int index;
    private final String title;

    public TabSelectEvent(TabControl tabControl, int index, String title) {
        this.control = tabControl;
        this.index = index;
        this.title = title;
    }

    @Override
    public TabControl getControl() {
        return control;
    }

    public int getIndex() {
        return index;
    }

    public String getTitle() {
        return title;
    }

    public String toString() {
        return "Event: Tab "+index+" Selected";
    }

}
