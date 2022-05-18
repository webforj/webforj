package org.dwcj.events;

import org.dwcj.controls.AbstractDwcControl;
import org.dwcj.controls.HtmlContainer;

public final class PageLoadedEvent implements IDwcEvent {

    private final HtmlContainer control;

    public PageLoadedEvent(HtmlContainer h) {
        this.control = h;
    }

    @Override
    public HtmlContainer getControl() {
        return control;
    }
}
