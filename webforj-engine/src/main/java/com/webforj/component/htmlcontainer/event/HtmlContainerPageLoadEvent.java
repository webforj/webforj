package com.webforj.component.htmlcontainer.event;

import com.webforj.component.ControlEvent;
import com.webforj.component.htmlcontainer.HtmlContainer;

public final class HtmlContainerPageLoadEvent implements ControlEvent {

  private final HtmlContainer control;

  public HtmlContainerPageLoadEvent(HtmlContainer h) {
    this.control = h;
  }

  @Override
  public HtmlContainer getControl() {
    return control;
  }
}
