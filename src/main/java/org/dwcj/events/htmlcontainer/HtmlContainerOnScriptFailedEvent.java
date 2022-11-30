package org.dwcj.events.htmlcontainer;

import org.dwcj.controls.HtmlContainer;
import org.dwcj.events.IDwcEvent;

public class HtmlContainerOnScriptFailedEvent implements IDwcEvent {
    
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
