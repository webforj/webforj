package org.dwcj.component.window.event;

import org.dwcj.component.ComponentEvent;
import org.dwcj.component.window.Panel;

public final class WindowClickEvent implements ComponentEvent {
    private final Panel control;

    public WindowClickEvent(Panel div) {
        this.control = div;
    }

    @Override
    public Panel getControl() {
        return control;
    }
}
