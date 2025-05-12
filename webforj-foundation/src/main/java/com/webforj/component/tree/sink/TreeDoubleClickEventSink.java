package com.webforj.component.tree.sink;

import com.basis.bbj.proxyif.SysGuiEventConstants;
import com.webforj.component.tree.Tree;
import com.webforj.component.tree.event.TreeDoubleClickEvent;
import com.webforj.dispatcher.EventDispatcher;
import java.util.Map;

/**
 * This class will map the BBjTreeMouseDoubleClickEvent event to a {@link TreeDoubleClickEvent}.
 *
 * @author Hyyan Abo Fakher
 * @since 25.01
 */
public class TreeDoubleClickEventSink extends TreeMouseEventSink {

  /**
   * Constructs a new {@code TreeClickEventSink}.
   *
   * @param component the Tree component
   * @param dispatcher the EventDispatcher
   */
  public TreeDoubleClickEventSink(Tree component, EventDispatcher dispatcher) {
    super(component, dispatcher, SysGuiEventConstants.ON_TREE_DOUBLE_CLICK);
  }

  @Override
  void doHandleEvent(Map<String, Object> eventMap) {
    TreeDoubleClickEvent javaEv = new TreeDoubleClickEvent((Tree) getComponent(), eventMap);
    getEventDispatcher().dispatchEvent(javaEv);
  }
}
