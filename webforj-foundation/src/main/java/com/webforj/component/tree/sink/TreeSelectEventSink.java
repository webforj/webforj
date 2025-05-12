package com.webforj.component.tree.sink;

import java.util.HashMap;
import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxies.event.BBjTreeNodeSelectedEvent;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import com.webforj.component.event.sink.AbstractDwcEventSink;
import com.webforj.component.tree.Tree;
import com.webforj.component.tree.event.TreeSelectEvent;
import com.webforj.dispatcher.EventDispatcher;

/**
 * This class will map the BBjTreeNodeSelectedEvent event to a {@link TreeSelectEventSink}.
 *
 * @author Hyyan Abo Fakher
 * @since 25.01
 */
public class TreeSelectEventSink extends AbstractDwcEventSink {

  /**
   * Constructs a new TreeSelectEventSink with the given component and dispatcher.
   *
   * @param component the DwcList component
   * @param dispatcher the EventDispatcher
   */
  public TreeSelectEventSink(Tree component, EventDispatcher dispatcher) {
    super(component, dispatcher, SysGuiEventConstants.ON_TREE_SELECT);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void handleEvent(BBjEvent ev) {
    BBjTreeNodeSelectedEvent event = (BBjTreeNodeSelectedEvent) ev;
    HashMap<String, Object> map = new HashMap<>();
    map.put("id", event.getNodeID());

    TreeSelectEvent javaEv = new TreeSelectEvent((Tree) getComponent(), map);
    getEventDispatcher().dispatchEvent(javaEv);
  }
}
