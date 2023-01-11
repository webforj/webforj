package org.dwcj.controls.htmlcontainer.events;

import org.dwcj.controls.htmlcontainer.HtmlContainer;
import org.dwcj.interfaces.ControlEvent;

import java.util.Map;

public final class HtmlContainerJavascriptEvent implements ControlEvent {

    private final HtmlContainer control;

    private final Map<String,String> eventMap;

    public HtmlContainerJavascriptEvent(HtmlContainer h, Map eventMap) { //NOSONAR
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