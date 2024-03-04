package com.webforj.component.htmlcontainer.event;

import com.webforj.component.ControlEvent;
import com.webforj.component.htmlcontainer.HtmlContainer;

public class HtmlContainerScriptLoadEvent implements ControlEvent {

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
