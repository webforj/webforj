package org.dwcj.component.htmlcontainer.event;

import org.dwcj.component.htmlcontainer.HtmlContainer;
import org.dwcj.interfaces.ComponentEvent;

public final class HtmlContainerPageLoadedEvent implements ComponentEvent {

    private final HtmlContainer control;

    public HtmlContainerPageLoadedEvent(HtmlContainer h) {
        this.control = h;
    }

    @Override
    public HtmlContainer getControl() {
        return control;
    }
}
