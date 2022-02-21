package org.dwcj.events;

import org.dwcj.controls.AbstractDwcControl;
import org.dwcj.controls.HtmlContainer;

public class JavascriptEvent implements IDwcEvent {

    private final HtmlContainer control;

    public JavascriptEvent(HtmlContainer h) {
        this.control = h;
    }

    @Override
    public AbstractDwcControl getControl() {
        return control;
    }
}