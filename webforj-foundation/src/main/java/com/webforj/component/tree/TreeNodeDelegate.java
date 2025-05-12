package com.webforj.component.tree;

import java.util.List;
import java.util.Optional;

/**
 * Represents a node in a tree structure.
 *
 * @author Hyyan Abo Fakher
 * @since 25.01
 */
public interface TreeNodeDelegate {

  /**
   * Returns the key that uniquely identifies this node.
   *
   * @return the non-null key of this node
   */
  Object getKey();

  /**
   * Returns the framework-assigned numeric id of this node.
   *
   * <p>
   * Unlike {@link #getKey()}, this value is generated internally and is guaranteed to be unique.
   * </p>
   *
   * @return the numeric id of this node
   */
  int getUniqueId();

  /**
   * Returns the display text of this node.
   *
   * @return the non-null text of this node
   */
  String getText();

  /**
   * Updates the display text of this node. Fires a property change event named {@code "text"}.
   *
   * @param text the new non-null text
   *
   * @return this node instance
   * @throws NullPointerException if {@code text} is null
   */
  TreeNodeDelegate setText(String text);

  /**
   * Returns the parent of this node, if any.
   *
   * @return an {@code Optional} describing the parent, or empty if root
   */
  Optional<TreeNode> getParent();

  /**
   * Returns an unmodifiable list of this node's children.
   *
   * @return the list of child nodes, never null
   */
  List<TreeNode> getChildren();

  /**
   * Inserts the specified node as a child at the given index.
   *
   * @param index the position at which to insert (0-based)
   * @param child the non-null node to insert
   *
   * @return this node instance
   * @throws IndexOutOfBoundsException if {@code index} is out of range
   * @throws IllegalArgumentException if {@code child} is null or already has a parent
   */
  TreeNodeDelegate insert(int index, TreeNode child);

  /**
   * Inserts a new child node with the given text at the specified index.
   *
   * @param index the position at which to insert (0-based)
   * @param text the non-null text of the new child node
   *
   * @return the newly created child node
   * @throws IndexOutOfBoundsException if {@code index} is out of range
   * @throws NullPointerException if {@code text} is null
   */
  TreeNode insert(int index, String text);

  /**
   * Inserts the specified nodes as children at the given index.
   *
   * @param index the position at which to insert (0-based)
   * @param nodes the non-null array of child nodes to insert
   *
   * @return this node instance
   * @throws NullPointerException if {@code nodes} is null or contains a null element
   * @throws IllegalArgumentException if any {@code node} already has a parent
   */
  TreeNodeDelegate insert(int index, TreeNode... nodes);

  /**
   * Inserts new child nodes with the given texts at the specified index.
   *
   * @param index the position at which to insert (0-based)
   * @param texts the non-null array of texts for new child nodes
   *
   * @return this node instance
   * @throws NullPointerException if {@code texts} is null or contains a null element
   */
  TreeNodeDelegate insert(int index, String... texts);

  /**
   * Adds the specified node as a child of this node.
   *
   * @param child the non-null node to add
   *
   * @return this node instance
   * @throws IllegalArgumentException if {@code child} is null or already has a parent
   */
  TreeNodeDelegate add(TreeNode child);

  /**
   * Adds a new child node with the given text.
   *
   * @param text the non-null text of the new child node
   *
   * @return the newly created child node
   * @throws NullPointerException if {@code text} is null
   */
  TreeNode add(String text);

  /**
   * Adds the specified nodes as children of this node.
   *
   * @param nodes the non-null array of child nodes to add
   * @return this node instance
   *
   * @throws NullPointerException if {@code nodes} is null or contains a null element
   * @throws IllegalArgumentException if any {@code node} already has a parent
   */
  TreeNodeDelegate add(TreeNode... nodes);

  /**
   * Adds new child nodes with the given texts.
   *
   * @param texts the non-null array of texts for new child nodes
   *
   * @return this node instance
   * @throws NullPointerException if {@code texts} is null or contains a null element
   */
  TreeNodeDelegate add(String... texts);

  /**
   * Removes the specified node from this node's children.
   *
   * @param child the child to remove
   */
  void remove(TreeNode child);

  /**
   * Removes all children from this node.
   */
  void removeAll();
}
