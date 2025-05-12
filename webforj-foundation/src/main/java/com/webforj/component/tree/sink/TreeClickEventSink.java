package com.webforj.component.tree.sink;

import java.util.Map;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import com.webforj.component.tree.Tree;
import com.webforj.component.tree.event.TreeClickEvent;
import com.webforj.dispatcher.EventDispatcher;

/**
 * This class will map the BBjTreeMouseUpEvent event to a {@link TreeClickEvent}.
 *
 * @author Hyyan Abo Fakher
 * @since 25.01
 */
public class TreeClickEventSink extends TreeMouseEventSink {

  /**
   * Constructs a new {@code TreeClickEventSink}.
   *
   * @param component the Tree component
   * @param dispatcher the EventDispatcher
   */
  public TreeClickEventSink(Tree component, EventDispatcher dispatcher) {
    super(component, dispatcher, SysGuiEventConstants.ON_TREE_MOUSE_UP);
  }

  @Override
  void doHandleEvent(Map<String, Object> eventMap) {
    TreeClickEvent javaEv = new TreeClickEvent((Tree) getComponent(), eventMap);
    getEventDispatcher().dispatchEvent(javaEv);
  }
}
