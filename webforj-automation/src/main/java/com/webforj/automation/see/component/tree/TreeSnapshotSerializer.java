package com.webforj.automation.see.component.tree;

import com.google.auto.service.AutoService;
import com.webforj.automation.see.SnapshotNode;
import com.webforj.automation.see.SnapshotSerializer;
import com.webforj.component.tree.Tree;
import com.webforj.component.tree.TreeNode;
import com.webforj.concern.HasText;
import java.util.List;
import java.util.Set;

/**
 * Serializes a tree with its text and its nodes with their keys, and the selected and expanded
 * nodes when the snapshot includes the browser state.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
@AutoService(SnapshotSerializer.class)
public final class TreeSnapshotSerializer implements SnapshotSerializer<Tree> {

  /**
   * {@inheritDoc}
   */
  @Override
  public Class<Tree> getContract() {
    return Tree.class;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Set<Class<?>> getServerHeldContracts() {
    return Set.of(HasText.class);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void describe(Tree tree, SnapshotNode node) {
    if (tree.getSelectionMode() == Tree.SelectionMode.MULTIPLE) {
      node.addFlag("multiple");
    }

    describe(tree, tree.getRoot().getChildren(), node, node.isBrowserState());
  }

  private void describe(Tree tree, List<TreeNode> nodes, SnapshotNode parent,
      boolean browserState) {
    List<TreeNode> selected = browserState ? tree.getSelectedItems() : List.of();
    parent.setItemCount(nodes.size());
    for (TreeNode treeNode : nodes.subList(0, Math.min(nodes.size(), parent.getMaxWidth() + 1))) {
      SnapshotNode line = parent.addItem("TreeNode").setText(treeNode.getText()).setAttribute("key",
          treeNode.getKey());
      if (selected.contains(treeNode)) {
        line.addFlag("selected");
      }

      if (browserState && tree.isExpanded(treeNode)) {
        line.addFlag("expanded");
      }

      describe(tree, treeNode.getChildren(), line, browserState);
    }
  }
}
