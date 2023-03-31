package org.dwcj.component.htmlcontainer.event;

import org.dwcj.component.htmlcontainer.HtmlContainer;
import org.dwcj.interfaces.ControlEvent;

public class HtmlContainerScriptLoadedEvent implements ControlEvent {
    
    private final HtmlContainer control;

    public HtmlContainerScriptLoadedEvent(HtmlContainer cHtmlContainer) {
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
        return "Event: HtmlContainer Script Loaded";
    }

}
