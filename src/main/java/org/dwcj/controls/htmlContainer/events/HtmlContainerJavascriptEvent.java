package org.dwcj.controls.htmlContainer.events;

import org.dwcj.controls.htmlContainer.HtmlContainer;
import org.dwcj.interfaces.IDwcEvent;

public final class HtmlContainerJavascriptEvent implements IDwcEvent {

    private final HtmlContainer control;

    public HtmlContainerJavascriptEvent(HtmlContainer h) {
        this.control = h;
    }

    @Override
    public HtmlContainer getControl() {
        return control;
    }
}