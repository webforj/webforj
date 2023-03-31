package org.dwcj.component.htmlcontainer.event;

import org.dwcj.component.ComponentEvent;
import org.dwcj.component.htmlcontainer.HtmlContainer;

public final class HtmlContainerPageLoadEvent implements ComponentEvent {

    private final HtmlContainer control;

    public HtmlContainerPageLoadEvent(HtmlContainer h) {
        this.control = h;
    }

    @Override
    public HtmlContainer getControl() {
        return control;
    }
}
