package org.dwcj.component.list.sink;

import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxies.event.BBjListClickEvent;
import com.basis.bbj.proxies.event.BBjListSelectEvent;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import java.util.HashMap;
import org.dwcj.component.event.EventDispatcher;
import org.dwcj.component.event.sink.AbstractDwcEventSink;
import org.dwcj.component.list.DwcList;
import org.dwcj.component.list.DwcSelectDropdown;
import org.dwcj.component.list.event.ListSelectEvent;

/**
 * This class will map the BBjListSelectEvent event to a {@link ListSelectEvent}.
 *
 * @author Hyyan Abo Fakher
 * @since 23.05
 */
public final class ListSelectEventSink extends AbstractDwcEventSink {

  /**
   * Constructs a new ListSelectEventSink with the given component and dispatcher.
   *
   * @param component the DwcList component
   * @param dispatcher the EventDispatcher
   */
  public ListSelectEventSink(DwcList<?> component, EventDispatcher dispatcher) {
    super(component, dispatcher,
        component instanceof DwcSelectDropdown ? SysGuiEventConstants.ON_LIST_SELECT
            // Scrolling through a BBjListBox with the arrow keys (and related keys like Page Up,
            // Page Down, Home, End) fires a BBjListClickEvent in DWC client. It is an odd legacy of
            // the original Visual PRO/5 implementation and an event with a name that doesn't
            // entirely reflect what it does
            : SysGuiEventConstants.ON_LIST_CLICK);
  }

  /**
   * Handles the BBj event and dispatches a new {@link ListSelectEvent}.
   *
   * @param ev the BBj event
   */
  @Override
  public void handleEvent(BBjEvent ev) {
    if (ev instanceof BBjListClickEvent) {
      handleListClickEvent(ev);
    } else if (ev instanceof BBjListSelectEvent) {
      handleListSelectEvent(ev);
    }
  }

  /**
   * Handles the BBjListClickEvent event and dispatches a new {@link ListSelectEvent}.
   *
   * @param ev the BBj event
   */
  private void handleListClickEvent(BBjEvent ev) {
    BBjListClickEvent event = (BBjListClickEvent) ev;
    HashMap<String, Object> map = new HashMap<>();

    map.put("index", event.getSelectedIndex());
    map.put("indices", event.getSelectedIndices());

    ListSelectEvent javaEv = new ListSelectEvent((DwcList<?>) getComponent(), map);
    getEventDispatcher().dispatchEvent(javaEv);
  }

  /**
   * Handles the BBjListSelectEvent event and dispatches a new {@link ListSelectEvent}.
   *
   * @param ev the BBj event
   */
  private void handleListSelectEvent(BBjEvent ev) {
    BBjListSelectEvent event = (BBjListSelectEvent) ev;
    HashMap<String, Object> map = new HashMap<>();

    map.put("index", event.getSelectedIndex());
    map.put("indices", event.getSelectedIndices());

    ListSelectEvent javaEv = new ListSelectEvent((DwcSelectDropdown<?>) getComponent(), map);
    getEventDispatcher().dispatchEvent(javaEv);
  }
}
