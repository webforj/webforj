package org.dwcj.component.htmlcontainer.event;

import org.dwcj.component.ControlEvent;
import org.dwcj.component.htmlcontainer.HtmlContainer;

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
