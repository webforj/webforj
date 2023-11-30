package org.dwcj.component.element.sink;

import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import java.util.HashMap;
import org.dwcj.component.element.Element;
import org.dwcj.component.element.event.ElementDefinedEvent;
import org.dwcj.component.event.sink.AbstractDwcEventSink;
import org.dwcj.dispatcher.EventDispatcher;

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
