package com.webforj.component.tree.event;

import com.webforj.component.tree.Tree;
import java.util.Map;

/**
 * An event which is fired when a tree node is collapsed.
 *
 * <p>
 * This event can be fired when a node is collapsed via user interaction.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 25.01
 */
public class TreeCollapseEvent extends TreeEvent {

  /**
   * Creates a new tree collapse event.
   *
   * @param component the component
   * @param eventMap the event map
   */
  public TreeCollapseEvent(Tree component, Map<String, Object> eventMap) {
    super(component, eventMap);
  }
}
