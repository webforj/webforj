package com.webforj.component.tree.event;

import com.webforj.component.tree.Tree;
import java.util.Map;

/**
 * An event which is fired when a tree node is double-clicked.
 *
 * @author Hyyan Abo Fakher
 * @since 25.01
 */
public class TreeDoubleClickEvent extends TreeMouseEvent {

  /**
   * Creates a new tree double-click event.
   *
   * @param component the component
   * @param eventMap the event map
   */
  public TreeDoubleClickEvent(Tree component, Map<String, Object> eventMap) {
    super(component, eventMap);
  }
}
