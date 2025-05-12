package com.webforj.component.tree.event;

import java.util.Map;
import java.util.Optional;
import com.webforj.component.event.ComponentEvent;
import com.webforj.component.tree.Tree;
import com.webforj.component.tree.TreeNode;

/**
 * An event which is fired when the user interacts with a tree node.
 *
 * @author Hyyan Abo Fakher
 * @since 25.01
 */
class TreeEvent extends ComponentEvent<Tree> {
  private int nodeId = -1;

  /**
   * Creates a new tree event.
   *
   * @param component the component
   * @param eventMap the event map
   */
  protected TreeEvent(Tree component, Map<String, Object> eventMap) {
    super(component, eventMap);
    Object id = getEventMap().get("id");
    if (id != null) {
      nodeId = (int) id;
    }
  }

  /**
   * Get the node that associated with this event.
   *
   * @return the node associated with this event, or {@code null} if the node is not found
   */
  public TreeNode getNode() {
    if (nodeId == -1) {
      return null;
    }

    Optional<TreeNode> node = Tree.findById(getComponent().getRoot(), nodeId);
    return node.orElse(null);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Tree getComponent() {
    return (Tree) super.getComponent();
  }
}
