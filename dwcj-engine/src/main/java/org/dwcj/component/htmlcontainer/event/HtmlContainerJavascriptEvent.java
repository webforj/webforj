package org.dwcj.component.htmlcontainer.event;

import org.dwcj.component.htmlcontainer.HtmlContainer;
import org.dwcj.interfaces.ComponentEvent;

import java.util.Map;

public final class HtmlContainerJavascriptEvent implements ComponentEvent {

    private final HtmlContainer control;

    private final Map<String,String> eventMap;

    public HtmlContainerJavascriptEvent(HtmlContainer h, Map<String,String> eventMap) {
        this.control = h;
        this.eventMap = eventMap;
    }

    @Override
    public HtmlContainer getControl() {
        return control;
    }

    public Map<String,String> getEventMap() {
        return eventMap;
    }
}