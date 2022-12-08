package org.dwcj.controls.htmlcontainer.events;

import org.dwcj.controls.htmlcontainer.HtmlContainer;
import org.dwcj.interfaces.IDwcEvent;

public final class HtmlContainerPageLoadedEvent implements IDwcEvent {

    private final HtmlContainer control;

    public HtmlContainerPageLoadedEvent(HtmlContainer h) {
        this.control = h;
    }

    @Override
    public HtmlContainer getControl() {
        return control;
    }
}
