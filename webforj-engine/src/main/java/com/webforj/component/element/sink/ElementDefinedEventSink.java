package com.webforj.component.element.sink;

import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import com.webforj.component.element.Element;
import com.webforj.component.element.event.ElementDefinedEvent;
import com.webforj.component.event.sink.AbstractDwcEventSink;
import com.webforj.dispatcher.EventDispatcher;
import java.util.HashMap;

/**
 * This class will map the {@code BBjWebComponentDefinedEvent} event to a Java
 * {@link ElementDefinedEvent}.
 *
 * @author Hyyan Abo Fakher
 * @since 23.06
 */
public final class ElementDefinedEventSink extends AbstractDwcEventSink {

  public ElementDefinedEventSink(Element element, EventDispatcher dispatcher) {
    super(element, dispatcher, SysGuiEventConstants.ON_DEFINED);
  }

  /**
   * Handles the BBj event and dispatches a new {@link ElementDefinedEvent}.
   *
   * @param ev the BBj event
   */
  @Override
  public void handleEvent(BBjEvent ev) {
    ElementDefinedEvent dispatchedEvent =
        new ElementDefinedEvent((Element) getComponent(), new HashMap<>());
    this.getEventDispatcher().dispatchEvent(dispatchedEvent);
  }
}
