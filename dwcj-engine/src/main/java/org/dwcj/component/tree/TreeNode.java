package org.dwcj.component.tree;

import java.util.LinkedList;
import java.util.List;

/**
 * Represents a node in a tree.
 */
public class TreeNode {

  public final int id;
  private String text;
  private TreeNode parent;
  private List<TreeNode> children;

  private boolean selected = false;
  private boolean expanded = false;
  private boolean expandable = false;
  private boolean editable = false;
  

  /**
   * Default constructor.
   *
   * @param id the id of the node
   * @param parent the parent of this node
   * @param text the text of the node
   */
  public TreeNode(int id, TreeNode parent, String text) {
    this.id = id;
    this.parent = parent;
    this.text = text;
    this.children = new LinkedList<>();
  }

  public static TreeNode root(int id, String text) {
    return new TreeNode(id, null, text);
  }

  /**
   * Adds a child to this node.
   *
   * @param id the id of the child
   * @param text the text of the child
   * @return the child
   */
  public TreeNode addChild(int id, String text) {
    TreeNode childNode = new TreeNode(id, this, text);
    this.children.add(childNode);
    return childNode;
  }

  /**
   * Creates a child and inserts it at the given index.
   *
   * @param id the id of the child
   * @param index the insertion index
   * @param text the text of the child
   * @return the child
   */
  public TreeNode insertChild(int id, int index, String text) {
    TreeNode childNode = new TreeNode(id, this, text);
    this.children.add(index, childNode);
    return childNode;
  }
 
  /**
   * Creates a child and sets it expandable.
   *
   * @param id the id of the child
   * @param text the text of the child
   * @return the child
   */
  public TreeNode addExpandableChild(int id, String text) {
    TreeNode childNode = new TreeNode(id, this, text);
    childNode.setExpandable(true);
    this.children.add(childNode);
    return childNode;
  }

  /**
   * Creates a expandable child and inserts it at the given index.
   *
   * @param id the id of the child
   * @param index the insertion index
   * @param text the text of the child
   * @return the child
   */
  public TreeNode insertExpandableChild(int id, int index, String text) {
    TreeNode chidlNode = new TreeNode(id, this, text);
    chidlNode.setExpandable(true);
    this.children.add(index, chidlNode);
    return chidlNode;
  }

  /**
   * Returns the TreeNode at the given index. 
   *
   * @param index the index
   * @return the TreeNode or null if not found
   */
  public TreeNode getChildAt(int index) {
    if (!children.isEmpty()) {
      return children.get(index);
    }
    return null;
  }

  /**
   * Returns the index of a child with the given id.
   *
   * @param id the id of the child
   * @return the index or -1 if not found
   */
  public int getIndexOfChild(int id) {
    if (children.isEmpty()) {
      return -1;
    }

    for (int i = 0; i < this.children.size(); i++) {
      final TreeNode node = this.children.get(i);
      if (node.id == id) {
        return i;
      }
    }
    return -1;
  }

  /**
   * Deletes a child with the given id.
   *
   * @param id the id of the child
   */
  public void removeChild(int id) {
    if (this.children.isEmpty()) {
      return;
    }

    for (int i = this.children.size() - 1; i >= 0; i--) {
      final TreeNode node = this.children.get(i);
      if (node.id == id) {
        this.children.remove(i);
        return;
      }
    }
  }
  
  /**
   * Searches for a node with the given id.
   *
   * @param node the node from which the search starts
   * @param id the id of the node in search
   * @return the node or null if not found
   */
  public TreeNode findNode(TreeNode node, int id) {
    if (node.id == id) {
      return node;
    } else {
      for (TreeNode child : node.children) {
        TreeNode result = findNode(child, id);
        if (result != null) {
          return result;
        }
      }
    }
    return null;
  }

  public void setSelected(boolean selected) {
    this.selected = selected;
  }

  public void setExpandable(boolean enabled) {
    this.expandable = enabled;
  }

  public void setEditable(boolean editable) {
    this.editable = editable;
  }

  public void setExpanded(boolean expanded) {
    this.expanded = expanded;
  }

  public void removeAllChildren() {
    this.children.clear();
  }

  public int getChildCount() {
    return this.children.size();
  }
  
  public boolean isLeafNode() {
    return this.children.isEmpty();
  }

  public TreeNode getParent() {
    return this.parent;
  }

  public String getText() {
    return this.text;
  }

  public boolean isSelected() {
    return this.selected;
  }

  public boolean isExpandable() {
    return this.expandable;
  }

  public boolean isExpanded() {
    return this.expanded;
  }

  public boolean isEditable() {
    return this.editable;
  }

}