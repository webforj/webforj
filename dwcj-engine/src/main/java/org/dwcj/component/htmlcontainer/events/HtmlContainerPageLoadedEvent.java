package org.dwcj.component.htmlcontainer.events;

import org.dwcj.component.htmlcontainer.HtmlContainer;
import org.dwcj.interfaces.ControlEvent;

public final class HtmlContainerPageLoadedEvent implements ControlEvent {

    private final HtmlContainer control;

    public HtmlContainerPageLoadedEvent(HtmlContainer h) {
        this.control = h;
    }

    @Override
    public HtmlContainer getControl() {
        return control;
    }
}