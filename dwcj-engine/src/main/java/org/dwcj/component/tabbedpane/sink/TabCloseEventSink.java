package org.dwcj.component.tabbedpane.sink;

import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxies.event.BBjTabCloseEvent;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import java.util.HashMap;
import org.dwcj.component.event.sink.AbstractDwcEventSink;
import org.dwcj.component.list.event.ListOpenEvent;
import org.dwcj.component.tabbedpane.TabbedPane;
import org.dwcj.component.tabbedpane.event.TabCloseEvent;
import org.dwcj.dispatcher.EventDispatcher;

/**
 * This class will map the BBjTabCloseEvent event to a {@link TabCloseEvent}.
 *
 * @author Hyyan Abo Fakher
 * @since 23.06
 */
public final class TabCloseEventSink extends AbstractDwcEventSink {

  /**
   * Constructs a new TabbedPaneSink.
   *
   * @param component the tabbed pane component
   * @param dispatcher the EventDispatcher
   */
  public TabCloseEventSink(TabbedPane component, EventDispatcher dispatcher) {
    super(component, dispatcher, SysGuiEventConstants.ON_TAB_CLOSE);
  }

  /**
   * Handles the BBj event and dispatches a new {@link ListOpenEvent}.
   *
   * @param ev the BBj event
   */
  @Override
  public void handleEvent(BBjEvent ev) {
    BBjTabCloseEvent event = (BBjTabCloseEvent) ev;
    HashMap<String, Object> map = new HashMap<>();

    map.put("index", event.getIndex());

    TabCloseEvent javaEv = new TabCloseEvent((TabbedPane) getComponent(), map);
    getEventDispatcher().dispatchEvent(javaEv);
  }
}
