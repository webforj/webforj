package com.webforj.component.list.sink;

import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxies.event.BBjListClickEvent;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import com.basis.tools.gui.ListItem;
import com.webforj.component.event.sink.AbstractDwcEventSink;
import com.webforj.component.list.DwcList;
import com.webforj.component.list.DwcSelectDropdown;
import com.webforj.component.list.event.ListClickEvent;
import com.webforj.dispatcher.EventDispatcher;
import java.util.HashMap;

/**
 * This class will map the BBjListClickEvent event to a {@link ListClickEvent}.
 *
 * @author Hyyan Abo Fakher
 * @since 23.05
 */
public final class ListClickEventSink extends AbstractDwcEventSink {

  /**
   * Constructs a new ListClickEventSink with the given component and dispatcher.
   *
   * @param component the DwcList component
   * @param dispatcher the EventDispatcher
   */
  public ListClickEventSink(DwcSelectDropdown<?> component, EventDispatcher dispatcher) {
    super(component, dispatcher, SysGuiEventConstants.ON_LIST_CLICK);
  }

  /**
   * Handles the BBj event and dispatches a new {@link ListClickEvent}.
   *
   * @param ev the BBj event
   */
  @Override
  public void handleEvent(BBjEvent ev) {
    BBjListClickEvent event = (BBjListClickEvent) ev;
    HashMap<String, Object> map = new HashMap<>();

    map.put("index", event.getSelectedIndex());
    map.put("indices", event.getSelectedIndices());

    ListClickEvent javaEv = new ListClickEvent((DwcSelectDropdown<?>) getComponent(), map);
    getEventDispatcher().dispatchEvent(javaEv);
  }
}
