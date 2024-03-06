package com.webforj.component.htmlcontainer.event;

import com.webforj.component.ControlEvent;
import com.webforj.component.htmlcontainer.HtmlContainer;

import java.util.Map;

public final class HtmlContainerJavascriptEvent implements ControlEvent {

  private final HtmlContainer control;

  private final Map<String, String> eventMap;

  public HtmlContainerJavascriptEvent(HtmlContainer h, Map<String, String> eventMap) {
    this.control = h;
    this.eventMap = eventMap;
  }

  @Override
  public HtmlContainer getControl() {
    return control;
  }

  public Map<String, String> getEventMap() {
    return eventMap;
  }
}
