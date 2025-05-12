package com.webforj.component.tree.sink;

import java.util.HashMap;
import java.util.Map;
import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxies.event.BBjTreeMouseEvent;
import com.webforj.component.event.sink.AbstractDwcEventSink;
import com.webforj.component.tree.Tree;
import com.webforj.dispatcher.EventDispatcher;

/**
 * The base class for all tree mouse event sinks.
 *
 * @author Hyyan Abo Fakher
 * @since 25.01
 */
public abstract class TreeMouseEventSink extends AbstractDwcEventSink {

  /**
   * Constructs a new TreeDoubleClickEventSink with the given component and dispatcher.
   *
   * @param component the Tree component
   * @param dispatcher the EventDispatcher
   * @param eventType the event type
   */
  protected TreeMouseEventSink(Tree component, EventDispatcher dispatcher, int eventType) {
    super(component, dispatcher, eventType);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void handleEvent(BBjEvent ev) {
    BBjTreeMouseEvent event = (BBjTreeMouseEvent) ev;
    int id = event.getExactNodeID();
    if (id == -1) {
      return;
    }

    HashMap<String, Object> map = new HashMap<>();
    map.put("id", id);

    map.put("mouseButton", event.getMouseButton());
    map.put("x", event.getX());
    map.put("y", event.getY());
    map.put("altDown", event.isAltDown());
    map.put("cmdDown", event.isCmdDown());
    map.put("controlDown", event.isControlDown());
    map.put("shiftDown", event.isShiftDown());

    doHandleEvent(map);
  }

  abstract void doHandleEvent(Map<String, Object> eventMap);
}
