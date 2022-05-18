package org.dwcj.events;

import org.dwcj.controls.AbstractDwcControl;
import org.dwcj.controls.HtmlContainer;

public final class JavascriptEvent implements IDwcEvent {

    private final HtmlContainer control;

    public JavascriptEvent(HtmlContainer h) {
        this.control = h;
    }

    @Override
    public HtmlContainer getControl() {
        return control;
    }
}