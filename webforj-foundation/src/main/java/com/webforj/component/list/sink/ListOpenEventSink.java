package com.webforj.component.list.sink;

import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxies.event.BBjListOpenEvent;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import com.webforj.component.event.sink.AbstractDwcEventSink;
import com.webforj.component.list.DwcList;
import com.webforj.component.list.DwcSelectDropdown;
import com.webforj.component.list.event.ListOpenEvent;
import com.webforj.dispatcher.EventDispatcher;
import java.util.HashMap;

/**
 * This class will map the BBjListOpenEvent event to a {@link ListOpenEvent}.
 *
 * @author Hyyan Abo Fakher
 * @since 23.05
 */
public final class ListOpenEventSink extends AbstractDwcEventSink {

  /**
   * Constructs a new ListOpenEventSink with the given component and dispatcher.
   *
   * @param component the DwcList component
   * @param dispatcher the EventDispatcher
   */
  public ListOpenEventSink(DwcSelectDropdown<?> component, EventDispatcher dispatcher) {
    super(component, dispatcher, SysGuiEventConstants.ON_LIST_OPEN);
  }

  /**
   * Handles the BBj event and dispatches a new {@link ListOpenEvent}.
   *
   * @param ev the BBj event
   */
  @Override
  public void handleEvent(BBjEvent ev) {
    BBjListOpenEvent event = (BBjListOpenEvent) ev;
    HashMap<String, Object> map = new HashMap<>();

    map.put("index", event.getSelectedIndex());
    map.put("indices", event.getSelectedIndices());

    ListOpenEvent javaEv = new ListOpenEvent((DwcSelectDropdown<?>) getComponent(), map);
    getEventDispatcher().dispatchEvent(javaEv);
  }
}
