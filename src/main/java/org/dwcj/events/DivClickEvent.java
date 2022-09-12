package org.dwcj.events;

import org.dwcj.controls.Label;
import org.dwcj.panels.AbstractDwcjPanel;
import org.dwcj.panels.Div;

public final class DivClickEvent implements IDwcEvent {
    private final Div control;

    public DivClickEvent(Div div) {
        this.control = div;
    }

    @Override
    public Div getControl() {
        return control;
    }
}
