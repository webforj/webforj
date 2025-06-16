package com.webforj.component.tree.sink;

import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxies.event.BBjTreeNodeExpandedEvent;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import com.webforj.component.event.sink.AbstractDwcEventSink;
import com.webforj.component.tree.Tree;
import com.webforj.component.tree.event.TreeExpandEvent;
import com.webforj.dispatcher.EventDispatcher;
import java.util.HashMap;

/**
 * This class will map the BBjTreeNodeExpandedEvent event to a {@link TreeExpandEvent}.
 *
 * @author Hyyan Abo Fakher
 * @since 25.01
 */
public class TreeExpandEventSink extends AbstractDwcEventSink {

  /**
   * Constructs a new TreeExpandEventSink with the given component and dispatcher.
   *
   * @param component the Tree component
   * @param dispatcher the EventDispatcher
   */
  public TreeExpandEventSink(Tree component, EventDispatcher dispatcher) {
    super(component, dispatcher, SysGuiEventConstants.ON_TREE_EXPAND);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void handleEvent(BBjEvent ev) {
    BBjTreeNodeExpandedEvent event = (BBjTreeNodeExpandedEvent) ev;
    HashMap<String, Object> map = new HashMap<>();
    map.put("id", event.getNodeID());

    TreeExpandEvent javaEv = new TreeExpandEvent((Tree) getComponent(), map);
    getEventDispatcher().dispatchEvent(javaEv);
  }
}
