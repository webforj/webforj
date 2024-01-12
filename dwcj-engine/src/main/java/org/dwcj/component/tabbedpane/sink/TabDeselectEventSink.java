package org.dwcj.component.tabbedpane.sink;

import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxies.event.BBjTabDeselectedEvent;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import java.util.HashMap;
import org.dwcj.component.event.sink.AbstractDwcEventSink;
import org.dwcj.component.list.event.ListOpenEvent;
import org.dwcj.component.tabbedpane.TabbedPane;
import org.dwcj.component.tabbedpane.event.TabDeselectEvent;
import org.dwcj.dispatcher.EventDispatcher;

/**
 * This class will map the BBjTabDeselectedEvent event to a {@link TabDeselectEvent}.
 *
 * @author Hyyan Abo Fakher
 * @since 23.06
 */
public final class TabDeselectEventSink extends AbstractDwcEventSink {

  /**
   * Constructs a new TabbedPaneSink.
   *
   * @param component the tabbed pane component
   * @param dispatcher the EventDispatcher
   */
  public TabDeselectEventSink(TabbedPane component, EventDispatcher dispatcher) {
    super(component, dispatcher, SysGuiEventConstants.ON_TAB_DESELECT);
  }

  /**
   * Handles the BBj event and dispatches a new {@link ListOpenEvent}.
   *
   * @param ev the BBj event
   */
  @Override
  public void handleEvent(BBjEvent ev) {
    BBjTabDeselectedEvent event = (BBjTabDeselectedEvent) ev;
    HashMap<String, Object> map = new HashMap<>();

    map.put("index", event.getIndex());

    TabDeselectEvent javaEv = new TabDeselectEvent((TabbedPane) getComponent(), map);
    getEventDispatcher().dispatchEvent(javaEv);
  }
}
