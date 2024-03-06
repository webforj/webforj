package com.webforj.component.element.event;

import com.webforj.component.element.Element;
import com.webforj.component.event.ComponentEvent;
import java.util.Map;

/**
 * Represents an event fired when an element is defined in the DOM.
 *
 * @author Hyyan Abo Fakher
 * @since 23.06
 */
public final class ElementDefinedEvent extends ComponentEvent<Element> {

  /**
   * Creates a new component event.
   *
   * @param element the component that fired the event
   * @param payload the event map
   */
  public ElementDefinedEvent(Element element, Map<String, Object> payload) {
    super(element, payload);
  }
}
