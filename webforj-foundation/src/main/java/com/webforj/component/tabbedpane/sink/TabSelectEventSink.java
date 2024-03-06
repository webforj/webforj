package com.webforj.component.tabbedpane.sink;

import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxies.event.BBjTabSelectedEvent;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import com.webforj.component.event.sink.AbstractDwcEventSink;
import com.webforj.component.list.event.ListOpenEvent;
import com.webforj.component.tabbedpane.TabbedPane;
import com.webforj.component.tabbedpane.event.TabSelectEvent;
import com.webforj.dispatcher.EventDispatcher;
import java.util.HashMap;

/**
 * This class will map the BBjTabSelectedEvent event to a {@link TabSelectEvent}.
 *
 * @author Hyyan Abo Fakher
 * @since 23.06
 */
public final class TabSelectEventSink extends AbstractDwcEventSink {

  /**
   * Constructs a new TabbedPaneSink.
   *
   * @param component the tabbed pane component
   * @param dispatcher the EventDispatcher
   */
  public TabSelectEventSink(TabbedPane component, EventDispatcher dispatcher) {
    super(component, dispatcher, SysGuiEventConstants.ON_TAB_SELECT);
  }

  /**
   * Handles the BBj event and dispatches a new {@link ListOpenEvent}.
   *
   * @param ev the BBj event
   */
  @Override
  public void handleEvent(BBjEvent ev) {
    BBjTabSelectedEvent event = (BBjTabSelectedEvent) ev;
    HashMap<String, Object> map = new HashMap<>();

    map.put("index", event.getIndex());

    TabSelectEvent javaEv = new TabSelectEvent((TabbedPane) getComponent(), map);
    getEventDispatcher().dispatchEvent(javaEv);
  }
}
