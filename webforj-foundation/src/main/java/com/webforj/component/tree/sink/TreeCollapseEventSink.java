package com.webforj.component.tree.sink;

import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxies.event.BBjTreeNodeCollapsedEvent;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import com.webforj.component.event.sink.AbstractDwcEventSink;
import com.webforj.component.tree.Tree;
import com.webforj.component.tree.event.TreeCollapseEvent;
import com.webforj.dispatcher.EventDispatcher;
import java.util.HashMap;

/**
 * This class will map the BBjTreeNodeCollapsedEvent event to a {@link TreeCollapseEvent}.
 *
 * @author Hyyan Abo Fakher
 * @since 25.01
 */
public class TreeCollapseEventSink extends AbstractDwcEventSink {

  /**
   * Constructs a new TreeCollapseEventSink with the given component and dispatcher.
   *
   * @param component the Tree component
   * @param dispatcher the EventDispatcher
   */
  public TreeCollapseEventSink(Tree component, EventDispatcher dispatcher) {
    super(component, dispatcher, SysGuiEventConstants.ON_TREE_COLLAPSE);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void handleEvent(BBjEvent ev) {
    BBjTreeNodeCollapsedEvent event = (BBjTreeNodeCollapsedEvent) ev;
    HashMap<String, Object> map = new HashMap<>();
    map.put("id", event.getNodeID());

    TreeCollapseEvent javaEv = new TreeCollapseEvent((Tree) getComponent(), map);
    getEventDispatcher().dispatchEvent(javaEv);
  }
}
