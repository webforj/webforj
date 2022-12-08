package org.dwcj.controls.htmlContainer.events;

import org.dwcj.controls.htmlContainer.HtmlContainer;
import org.dwcj.interfaces.IDwcEvent;

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
