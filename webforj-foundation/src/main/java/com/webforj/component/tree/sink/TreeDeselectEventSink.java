package com.webforj.component.tree.sink;

import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxies.event.BBjTreeNodeDeselectedEvent;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import com.webforj.component.event.sink.AbstractDwcEventSink;
import com.webforj.component.tree.Tree;
import com.webforj.component.tree.event.TreeDeselectEvent;
import com.webforj.dispatcher.EventDispatcher;
import java.util.HashMap;

/**
 * This class will map the BBjTreeNodeDeselectedEvent event to a {@link TreeDeselectEventSink}.
 *
 * @author Hyyan Abo Fakher
 * @since 25.01
 */
public class TreeDeselectEventSink extends AbstractDwcEventSink {

  /**
   * Constructs a new TreeDeselectEventSink with the given component and dispatcher.
   *
   * @param component the DwcList component
   * @param dispatcher the EventDispatcher
   */
  public TreeDeselectEventSink(Tree component, EventDispatcher dispatcher) {
    super(component, dispatcher, SysGuiEventConstants.ON_TREE_DESELECT);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void handleEvent(BBjEvent ev) {
    BBjTreeNodeDeselectedEvent event = (BBjTreeNodeDeselectedEvent) ev;
    HashMap<String, Object> map = new HashMap<>();
    map.put("id", event.getNodeID());

    TreeDeselectEvent javaEv = new TreeDeselectEvent((Tree) getComponent(), map);
    getEventDispatcher().dispatchEvent(javaEv);
  }
}
