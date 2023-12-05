package org.dwcj.component.element.event;

import java.util.Map;
import org.dwcj.component.element.Element;
import org.dwcj.component.event.ComponentEvent;

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
