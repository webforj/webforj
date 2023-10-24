package org.dwcj.component.list.sink;

import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxies.event.BBjListCloseEvent;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import java.util.HashMap;
import org.dwcj.component.event.EventDispatcher;
import org.dwcj.component.event.sink.AbstractDwcEventSink;
import org.dwcj.component.list.DwcSelectDropdown;
import org.dwcj.component.list.event.ListCloseEvent;

/**
 * This class will map the BBjListCloseEvent event to a {@link ListCloseEvent}.
 *
 * @author Hyyan Abo Fakher
 * @since 23.05
 */
public final class ListCloseEventSink extends AbstractDwcEventSink {

  /**
   * Constructs a new ListCloseEventSink with the given component and dispatcher.
   *
   * @param component the DwcList component
   * @param dispatcher the EventDispatcher
   */
  public ListCloseEventSink(DwcSelectDropdown<?> component, EventDispatcher dispatcher) {
    super(component, dispatcher, SysGuiEventConstants.ON_LIST_CLOSE);
  }

  /**
   * Handles the BBj event and dispatches a new {@link ListCloseEvent}.
   *
   * @param ev the BBj event
   */
  @Override
  public void handleEvent(BBjEvent ev) {
    BBjListCloseEvent event = (BBjListCloseEvent) ev;
    HashMap<String, Object> map = new HashMap<>();

    map.put("index", event.getSelectedIndex());
    map.put("indices", event.getSelectedIndices());

    ListCloseEvent javaEv = new ListCloseEvent((DwcSelectDropdown<?>) getComponent(), map);
    getEventDispatcher().dispatchEvent(javaEv);
  }
}
