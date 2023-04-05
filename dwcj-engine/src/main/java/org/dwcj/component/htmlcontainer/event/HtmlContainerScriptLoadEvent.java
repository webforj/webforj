package org.dwcj.component.htmlcontainer.event;

import org.dwcj.component.ComponentEvent;
import org.dwcj.component.htmlcontainer.HtmlContainer;

public class HtmlContainerScriptLoadEvent implements ComponentEvent {

  private final HtmlContainer control;

  public HtmlContainerScriptLoadEvent(HtmlContainer cHtmlContainer) {
    this.control = cHtmlContainer;
  }

  @Override
  public HtmlContainer getControl() {
    return control;
  }

  public String getUrl() {
    return this.control.getUrl();
  }

  public String toString() {
    return "Event: HtmlContainer Script Loaded";
  }

}
