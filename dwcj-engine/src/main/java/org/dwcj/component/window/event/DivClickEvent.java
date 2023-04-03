package org.dwcj.component.window.event;

import org.dwcj.component.ComponentEvent;
import org.dwcj.component.window.Div;

public final class DivClickEvent implements ComponentEvent {
    private final Div control;

    public DivClickEvent(Div div) {
        this.control = div;
    }

    @Override
    public Div getControl() {
        return control;
    }
}
