package org.dwcj.component.list.sink;

import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxies.event.BBjListOpenEvent;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import java.util.HashMap;
import org.dwcj.component.event.EventDispatcher;
import org.dwcj.component.event.sink.AbstractDwcEventSink;
import org.dwcj.component.list.DwcSelectDropdown;
import org.dwcj.component.list.event.ListOpenEvent;

/**
 * This class will map the BBjListOpenEvent event to a {@link ListOpenEvent}.
 *
 * @author Hyyan Abo Fakher
 * @since 23.05
 */
public final class ListOpenEventSink extends AbstractDwcEventSink {

  /**
   * Constructs a new ListSelectEventSink with the given component and dispatcher.
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
