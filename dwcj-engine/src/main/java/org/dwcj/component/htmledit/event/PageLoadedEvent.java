package org.dwcj.component.htmledit.event;

import java.util.Map;
import org.dwcj.component.AbstractComponent;
import org.dwcj.component.event.Event;

/**
 * An event that is fired when the htmlEdit loaded a page.
 */
public class PageLoadedEvent extends Event<AbstractComponent> {

  /**
   * Gets the text of the component that is sent as part of the event payload.
   *
   * @return the text of the component.
   */
  public String getText() {
    return (String) this.getEventMap().get("text");
  }

  /**
   * Gets the url of the component that is sent as part of the event payload.
   *
   * @return the url of the component.
   */
  public String getUrl() {
    return (String) this.getEventMap().get("url");
  }

  /**
   * Creates a new Page Loaded Event.
   *
   * @param component the component that fired the event
   * @param payload the event map
   */
  public PageLoadedEvent(AbstractComponent component, Map<String, Object> payload) {
    super(component, payload);
  }
}
