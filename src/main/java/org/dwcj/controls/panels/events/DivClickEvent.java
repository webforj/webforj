package org.dwcj.controls.panels.events;

import org.dwcj.controls.panels.Div;
import org.dwcj.interfaces.DwcEvent;

public final class DivClickEvent implements DwcEvent {
    private final Div control;

    public DivClickEvent(Div div) {
        this.control = div;
    }

    @Override
    public Div getControl() {
        return control;
    }
}
