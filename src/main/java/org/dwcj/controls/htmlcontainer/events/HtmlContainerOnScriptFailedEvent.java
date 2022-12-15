package org.dwcj.controls.htmlcontainer.events;

import org.dwcj.controls.htmlcontainer.HtmlContainer;
import org.dwcj.interfaces.ControlEvent;

public class HtmlContainerOnScriptFailedEvent implements ControlEvent {
    
    private final HtmlContainer control;

    public HtmlContainerOnScriptFailedEvent(HtmlContainer cHtmlContainer) {
        this.control = cHtmlContainer;
    }

    @Override
    public HtmlContainer getControl() {
        return control;
    }

    public String getUrl(){
        return this.control.getUrl();
    }

    public String toString() {
        return "Event: HtmlContainer Script Failed";
    }

}
