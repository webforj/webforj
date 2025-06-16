package com.webforj.component.tree.event;

import com.webforj.component.tree.Tree;
import java.util.Map;

/**
 * An event which is fired when a tree node is clicked.
 *
 * @author Hyyan Abo Fakher
 * @since 25.01
 */
public class TreeClickEvent extends TreeMouseEvent {

  /**
   * Creates a new tree click event.
   *
   * @param component the component
   * @param eventMap the event map
   */
  public TreeClickEvent(Tree component, Map<String, Object> eventMap) {
    super(component, eventMap);
  }
}
