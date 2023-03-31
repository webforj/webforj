package org.dwcj.component.panels.events;

import org.dwcj.component.panels.Div;
import org.dwcj.interfaces.ComponentEvent;

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
