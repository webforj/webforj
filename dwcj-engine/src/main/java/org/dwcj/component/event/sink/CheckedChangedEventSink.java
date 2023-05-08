package org.dwcj.component.event.sink;


import com.basis.bbj.proxies.event.BBjCheckOffEvent;
import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import java.util.HashMap;
import org.dwcj.Environment;
import org.dwcj.component.AbstractComponent;
import org.dwcj.component.AbstractDwcComponent;
import org.dwcj.component.event.CheckedChangedEvent;
import org.dwcj.component.event.CheckedEvent;
import org.dwcj.component.event.Event;
import org.dwcj.component.event.EventDispatcher;
import org.dwcj.component.event.UncheckedEvent;
import org.dwcj.exceptions.DwcjRuntimeException;

/**
 * This class will map the BBjCheckOnEvent event to a Java {@link CheckedEvent}.
 */
public class CheckedChangedEventSink extends AbstractSink {

  public CheckedChangedEventSink(AbstractDwcComponent component, EventDispatcher dispatcher) {
    super(component, dispatcher, SysGuiEventConstants.ON_CHECK_ON);
  }

  @Override
  public void setCallback() {
    try {
      control.setCallback(SysGuiEventConstants.ON_CHECK_ON,
          Environment.getInstance().getDwcjHelper().getEventProxy(this, "handleEvent"), "onEvent");
      control.setCallback(SysGuiEventConstants.ON_CHECK_OFF,
          Environment.getInstance().getDwcjHelper().getEventProxy(this, "handleOnEvent"),
          "onEvent");
    } catch (Exception e) {
      throw new DwcjRuntimeException("Failed to set callback.", e);
    }
  }

  @Override
  public void removeCallback() {
    try {
      control.clearCallback(SysGuiEventConstants.ON_CHECK_ON);
      control.clearCallback(SysGuiEventConstants.ON_CHECK_OFF);
    } catch (Exception e) {
      throw new DwcjRuntimeException("Failed to remove callback.", e);
    }
  }

  /**
   * Handles the BBj event and dispatches a new {@link CheckedChangedEvent} and
   * {@link UncheckedEvent}.
   *
   * @param ev A BBj check off event
   */
  public void handleEvent(BBjEvent ev) {
    HashMap<String, Object> map = new HashMap<>();
    CheckedChangedEvent dwcEv = new CheckedChangedEvent(component, map);
    this.dispatcher.dispatchEvent(dwcEv);
    Event<AbstractComponent> dwcEvent;
    dwcEvent = new UncheckedEvent(component, map);
    this.dispatcher.dispatchEvent(dwcEvent);
  }

  /**
   * Handles the BBj event and dispatches a new {@link CheckedChangedEvent} and
   * {@link CheckedEvent}.
   *
   * @param ev A BBj check on event
   */
  public void handleOnEvent(BBjEvent ev) {
    HashMap<String, Object> map = new HashMap<>();
    CheckedChangedEvent dwcEv = new CheckedChangedEvent(component, map);
    this.dispatcher.dispatchEvent(dwcEv);
    Event<AbstractComponent> dwcEvent;
    dwcEvent = new CheckedEvent(component, map);
    this.dispatcher.dispatchEvent(dwcEvent);
  }
}
