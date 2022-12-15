package org.dwcj.controls.htmlcontainer.events;

import org.dwcj.App;
import org.dwcj.controls.htmlcontainer.HtmlContainer;
import org.dwcj.interfaces.ControlEvent;

public class HtmlContainerOnScriptLoadedEvent implements ControlEvent {
    
    private final HtmlContainer control;

    public HtmlContainerOnScriptLoadedEvent(HtmlContainer cHtmlContainer) {
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
