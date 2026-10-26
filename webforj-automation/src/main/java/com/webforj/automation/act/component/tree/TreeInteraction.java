package com.webforj.automation.act.component.tree;

import com.google.auto.service.AutoService;
import com.webforj.automation.act.AbstractComponentInteraction;
import com.webforj.automation.act.ComponentInteraction;
import com.webforj.automation.act.DeselectInteraction;
import com.webforj.automation.act.DoubleClickInteraction;
import com.webforj.automation.act.ItemClickInteraction;
import com.webforj.automation.act.MouseOptions;
import com.webforj.automation.act.SelectInteraction;
import com.webforj.automation.act.exception.InteractionRejectedException;
import com.webforj.component.tree.Tree;
import com.webforj.component.tree.TreeNode;
import com.webforj.component.tree.event.TreeClickEvent;
import com.webforj.component.tree.event.TreeCollapseEvent;
import com.webforj.component.tree.event.TreeDeselectEvent;
import com.webforj.component.tree.event.TreeDoubleClickEvent;
import com.webforj.component.tree.event.TreeExpandEvent;
import com.webforj.component.tree.event.TreeSelectEvent;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Consumer;

/**
 * Selects, deselects, clicks, expands and collapses the nodes of trees.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
@AutoService(ComponentInteraction.class)
public final class TreeInteraction extends AbstractComponentInteraction<Tree>
    implements SelectInteraction<Tree, TreeNode>, DeselectInteraction<Tree, TreeNode>,
    ItemClickInteraction<Tree, TreeNode>, DoubleClickInteraction<Tree, TreeNode> {
  private static final String ID = "id";

  /**
   * Creates the interaction.
   */
  public TreeInteraction() {
    super(Tree.class);
  }

  /**
   * Returns the interaction that expands the given node of a tree.
   *
   * <pre>{@code
   * UserInteraction.perform(tree, TreeInteraction.expand(fruits))
   * }</pre>
   *
   * @param node the node to expand
   *
   * @return the interaction to perform on the tree
   * @throws InteractionRejectedException when performed, if the tree does not contain the node
   */
  public static Consumer<Tree> expand(TreeNode node) {
    return tree -> {
      Map<String, Object> data = createNodeData(tree, node);
      if (tree.isExpanded(node)) {
        return;
      }

      tree.expand(node);
      fire(new TreeExpandEvent(tree, data));
    };
  }

  /**
   * Returns the interaction that collapses the given node of a tree.
   *
   * @param node the node to collapse
   *
   * @return the interaction to perform on the tree
   * @throws InteractionRejectedException when performed, if the tree does not contain the node
   */
  public static Consumer<Tree> collapse(TreeNode node) {
    return tree -> {
      Map<String, Object> data = createNodeData(tree, node);
      if (!tree.isExpanded(node)) {
        return;
      }

      tree.collapse(node);
      fire(new TreeCollapseEvent(tree, data));
    };
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void select(Tree tree, TreeNode node) {
    Map<String, Object> data = createNodeData(tree, node);
    List<TreeNode> deselected = tree.getSelectionMode() == Tree.SelectionMode.SINGLE
        ? tree.getSelectedItems().stream().filter(selected -> selected != node).toList()
        : List.of();
    tree.select(node);
    for (TreeNode selected : deselected) {
      fire(new TreeDeselectEvent(tree, createNodeData(tree, selected)));
    }

    fire(new TreeSelectEvent(tree, data));
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void deselect(Tree tree, TreeNode node) {
    Map<String, Object> data = createNodeData(tree, node);
    tree.deselect(node);
    fire(new TreeDeselectEvent(tree, data));
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void click(Tree tree, TreeNode node, MouseOptions options) {
    requireShown(tree, node);
    fire(new TreeClickEvent(tree, createMouseData(tree, node, options)));
    selectClicked(tree, node);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void doubleClick(Tree tree, TreeNode node, MouseOptions options) {
    requireShown(tree, node);
    fire(new TreeDoubleClickEvent(tree, createMouseData(tree, node, options)));
    selectClicked(tree, node);
  }

  private static void requireShown(Tree tree, TreeNode node) {
    createNodeData(tree, node);
    for (TreeNode parent = node.getParent().orElse(null); parent != null
        && parent != tree.getRoot(); parent = parent.getParent().orElse(null)) {
      if (!tree.isExpanded(parent)) {
        throw reject(tree, "shows the node '" + node.getText() + "' inside the collapsed node '"
            + parent.getText() + "' and a user cannot click it");
      }
    }
  }

  private static void selectClicked(Tree tree, TreeNode node) {
    if (tree.getSelectedItems().contains(node)) {
      return;
    }

    Map<String, Object> data = createNodeData(tree, node);
    tree.select(node);
    fire(new TreeSelectEvent(tree, data));
  }

  private static Map<String, Object> createMouseData(Tree tree, TreeNode node,
      MouseOptions options) {
    Map<String, Object> data = mouseData(tree, options);
    data.putAll(createNodeData(tree, node));

    return data;
  }

  private static Map<String, Object> createNodeData(Tree tree, TreeNode node) {
    if (Tree.findById(tree.getRoot(), node.getUniqueId()).isEmpty()) {
      throw reject(tree, "does not contain the node '" + node.getText() + "'");
    }

    Map<String, Object> data = new HashMap<>();
    data.put(ID, node.getUniqueId());

    return data;
  }
}
