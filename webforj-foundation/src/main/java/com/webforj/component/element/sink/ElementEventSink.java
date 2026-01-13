package com.webforj.component.element.sink;

import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxies.event.BBjWebEvent;
import com.basis.bbj.proxies.event.BBjWebEventOptions;
import com.basis.bbj.proxies.sysgui.BBjControl;
import com.basis.bbj.proxies.sysgui.BBjWebComponent;
import com.basis.startup.type.BBjException;
import com.webforj.component.element.Element;
import com.webforj.component.element.event.DebouncePhase;
import com.webforj.component.element.event.ElementEvent;
import com.webforj.component.element.event.ElementEventOptions;
import com.webforj.component.event.sink.AbstractDwcEventSink;
import com.webforj.dispatcher.EventDispatcher;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

/**
 * Maps {@code BBjWebEvent} events to {@link ElementEvent} instances and handles event callbacks.
 *
 * <p>
 * This class serves as a bridge between {@code BBjWebEvent} events and custom {@code ElementEvent}
 * instances. It allows for the mapping of BBjWebEvent events to ElementEvent events and provides
 * the capability to handle multiple event callbacks.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 23.06
 */
public final class ElementEventSink extends AbstractDwcEventSink {
  private final String type;
  private Map<Integer, ElementEventOptions> callbackOptionsMap = new HashMap<>();

  /**
   * Constructs a new instance of {@link ElementEventSink}.
   *
   * @param component The associated Element component.
   * @param type The event type.
   * @param dispatcher The event dispatcher.
   */
  public ElementEventSink(Element component, String type, EventDispatcher dispatcher) {
    super(component, dispatcher, type);
    this.type = type;
  }

  /**
   * Handles the BBj event and dispatches a new {@link ElementEvent}.
   *
   * @param bbjEvent The BBj event to handle.
   */
  @Override
  public void handleEvent(BBjEvent bbjEvent) {
    BBjWebEvent bbjWebEvent = (BBjWebEvent) bbjEvent;
    ElementEvent event;
    int id = bbjWebEvent.getCallbackID();
    ElementEventOptions options = callbackOptionsMap.get(id);

    event =
        new ElementEvent((Element) getComponent(), bbjWebEvent.getEventMap(), type, id, options);
    getEventDispatcher().dispatchEvent(event);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean isMultipleCallbacks() {
    return true;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected String doSetCallback(BBjControl control, Object options, Object handler,
      String callback) throws BBjException {
    BBjWebComponent theControl = (BBjWebComponent) control;

    // create a control options and map it from the passed ElementEventOptions
    BBjWebEventOptions controlOptions = theControl.newEventOptions();
    ElementEventOptions elementOptions = (ElementEventOptions) options;

    // Set the code and filter
    controlOptions.setCode(elementOptions.getCode());
    controlOptions.setFilter(elementOptions.getFilter());

    // Add the items
    for (Entry<String, String> entry : elementOptions.getDataMap().entrySet()) {
      controlOptions.addItem(entry.getKey(), entry.getValue());
    }

    // Set the debounce/throttle options
    if (elementOptions.isDebounce()) {
      DebouncePhase phase = elementOptions.getDebouncePhase();
      boolean leading = phase.equals(DebouncePhase.LEADING) || phase.equals(DebouncePhase.BOTH);
      boolean trailing = phase.equals(DebouncePhase.TRAILING) || phase.equals(DebouncePhase.BOTH);

      controlOptions.setDebounce(elementOptions.getDebounceTimeout(), leading, trailing);
    } else if (elementOptions.isThrottle()) {
      controlOptions.setThrottle(elementOptions.getThrottleTimeout());
    } else {
      controlOptions.setImmediate();
    }

    int id = theControl.setCallback(type, handler, callback, controlOptions);
    callbackOptionsMap.put(id, elementOptions);

    return String.valueOf(id);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void doRemoveCallback(BBjControl control, String id) throws BBjException {
    int theId = Integer.parseInt(id);
    callbackOptionsMap.remove(theId);

    // Remove the callback only if there are no more callbacks registered
    BBjWebComponent theControl = (BBjWebComponent) control;
    theControl.clearCallback(type, theId);
  }
}
