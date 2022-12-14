package org.dwcj.controls.htmlcontainer.events;

import org.dwcj.controls.htmlcontainer.HtmlContainer;
import org.dwcj.interfaces.DwcEvent;

public final class HtmlContainerJavascriptEvent implements DwcEvent {

    private final HtmlContainer control;

    public HtmlContainerJavascriptEvent(HtmlContainer h) {
        this.control = h;
    }

    @Override
    public HtmlContainer getControl() {
        return control;
    }
}