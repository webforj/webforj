package com.webforj.component.tree.event;

import com.webforj.component.tree.Tree;
import java.util.Map;

/**
 * An event which is fired when the user deselects an node from a tree.
 *
 * <p>
 * This event can be fired when an node is clicked on, or navigated to via keyboard interactions.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 25.01
 */
public class TreeDeselectEvent extends TreeEvent {

  /**
   * Creates a new tree deselect event.
   *
   * @param component the component
   * @param eventMap the event map
   */
  public TreeDeselectEvent(Tree component, Map<String, Object> eventMap) {
    super(component, eventMap);
  }
}
