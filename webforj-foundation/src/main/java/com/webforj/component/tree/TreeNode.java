package com.webforj.component.tree;

import com.webforj.Environment;
import com.webforj.component.icons.IconDefinition;
import com.webforj.environment.ObjectTable;
import com.webforj.exceptions.WebforjRuntimeException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Consumer;

/**
 * Represents a single node in a tree structure.
 *
 * <p>
 * A TreeNode is used to define and organize hierarchical data within a {@link Tree} component. It
 * can be dynamically updated and responds to changes in the associated tree.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 25.01
 */
public final class TreeNode implements TreeNodeDelegate {
  private static final AtomicInteger ID_GENERATOR = new AtomicInteger();
  static final String LOOKUP_KEY = "com.webforj.component.tree.TreeNode.ID_SEQ";
  private static final String NULL_ICON_MESSAGE = "Icon cannot be null";
  private static final String NULL_SELECTED_ICON_MESSAGE = "Selected icon cannot be null";
  private final Object key;
  private final int id;
  private Tree tree;
  private String text = "";
  private String tooltipText = "";
  private String icon;
  private String selectedIcon;
  private TreeNode parent = null;
  private final List<TreeNode> children = new ArrayList<>();

  TreeNode(Tree tree) {
    this.key = "__ROOT__";
    this.id = getIdGenerator().getAndIncrement();
    setTree(tree);
  }

  /**
   * Constructs a new tree node with the given key, text, and expandable flag.
   *
   * @param key the non-null key identifying this node
   * @param text the display text of this node, may be empty but not null
   *
   * @throws IllegalArgumentException if {@code key} or {@code text} is null
   */
  public TreeNode(Object key, String text) {
    this.key = Objects.requireNonNull(key, "Key cannot be null");
    this.id = getIdGenerator().getAndIncrement();
    setText(text);
  }

  /**
   * Constructs a new tree node with the given text and expandable flag. The {@code key} defaults to
   * the {@code text} value.
   *
   * @param text the non-null display text and key for this node
   * @param expandable if {@code true}, the node is always shown expandable
   */
  public TreeNode(String text) {
    this(text, text);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Object getKey() {
    return key;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int getUniqueId() {
    return id;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getText() {
    return text;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public TreeNode setText(String text) {
    this.text = Objects.requireNonNull(text, "Text cannot be null");
    withTree(t -> t.doSetText(this));
    return this;
  }

  /**
   * Retrieves the tooltip text of the node
   *
   * @return the tooltip text of the node
   */
  public String getTooltipText() {
    return tooltipText;
  }

  /**
   * Sets the tooltip text of the node.
   *
   * @param tooltipText the tooltip text to set.
   * @return the node itself.
   */
  public TreeNode setTooltipText(String text) {
    this.tooltipText = Objects.requireNonNull(text, "Tooltip text cannot be null");
    withTree(t -> t.doSetTooltipText(this));
    return this;
  }

  /**
   * Sets the icon for this node.
   *
   * @param src The URL of the image. If a URL is provided and begins with {@code context://}, it
   *        will be resolved as a context URL, pointing to the root of your application's resources
   *        folder, and the image URL will be a base64-encoded string of the image. If a URL is
   *        provided and starts with {@code ws://}, it will be resolved as a web server URL,
   *        pointing to the root of the web server, and the image URL will be a fully qualified URL.
   *        if a URL is provided and starts with {@code icons://}, it will be resolved as an icons
   *        URL.
   *
   * @return the node itself
   * @throws NullPointerException if {@code icon} is null
   */
  public TreeNode setIcon(String src) {
    this.icon = Objects.requireNonNull(src, NULL_ICON_MESSAGE);
    withTree(t -> t.doSetIcon(this));
    return this;
  }

  /**
   * Sets the icon for this node using an IconDefinition.
   *
   * @param icon the IconDefinition to set
   *
   * @return the node itself
   * @throws NullPointerException if {@code icon} is null
   */
  public TreeNode setIcon(IconDefinition<?> icon) {
    Objects.requireNonNull(icon, NULL_ICON_MESSAGE);
    this.icon = String.format("%s:%s", icon.getPool(), icon.getName());
    withTree(t -> t.doSetIcon(this));
    return this;
  }

  /**
   * Gets the icon for this node.
   *
   * @return the icon, or null if not set
   */
  public String getIcon() {
    return icon;
  }

  /**
   * Sets the selected icon for this node.
   *
   * @param icon the selected icon to set
   *
   * @return the node itself
   * @throws NullPointerException if {@code selectedIcon} is null
   */
  public TreeNode setSelectedIcon(String icon) {
    this.selectedIcon = Objects.requireNonNull(icon, NULL_SELECTED_ICON_MESSAGE);
    withTree(t -> t.doSetSelectedIcon(this));
    return this;
  }

  /**
   * Sets the selected icon for this node using an IconDefinition.
   *
   * @param icon the IconDefinition to set
   *
   * @return the node itself
   * @throws NullPointerException if {@code icon} is null
   */
  public TreeNode setSelectedIcon(IconDefinition<?> icon) {
    Objects.requireNonNull(icon, NULL_SELECTED_ICON_MESSAGE);
    this.selectedIcon = String.format("%s:%s", icon.getPool(), icon.getName());
    withTree(t -> t.doSetSelectedIcon(this));
    return this;
  }

  /**
   * Gets the selected icon for this node.
   *
   * @return the selected icon, or null if not set
   */
  public String getSelectedIcon() {
    return selectedIcon;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Optional<TreeNode> getParent() {
    return Optional.ofNullable(parent);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public List<TreeNode> getChildren() {
    return Collections.unmodifiableList(children);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public TreeNode insert(int index, TreeNode child) {
    Objects.requireNonNull(child, "Child cannot be null");

    if (child.parent != null) {
      throw new IllegalArgumentException("Child already has a parent");
    }

    if (index < 0 || index > children.size()) {
      throw new IndexOutOfBoundsException(
          String.format("Index %d out of range (0..%d)", index, children.size()));
    }

    children.add(index, child);
    child.parent = this;
    child.setTree(tree);
    withTree(t -> t.doInsert(child));

    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public TreeNode insert(int index, String text) {
    TreeNode child = new TreeNode(text);
    this.insert(index, child);
    return child;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public TreeNode insert(int index, TreeNode... nodes) {
    for (TreeNode node : nodes) {
      insert(index++, node);
    }

    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public TreeNode insert(int index, String... texts) {
    for (String t : texts) {
      insert(index++, t);
    }

    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public TreeNode add(TreeNode child) {
    return insert(children.size(), child);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public TreeNode add(String text) {
    TreeNode child = new TreeNode(text);
    this.add(child);
    return child;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public TreeNode add(TreeNode... nodes) {
    return insert(children.size(), nodes);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public TreeNode add(String... texts) {
    return insert(children.size(), texts);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void remove(TreeNode child) {
    if (children.remove(child)) {
      withTree(t -> t.doRemove(child));
      child.parent = null;
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void removeAll() {
    withTree(t -> t.doRemoveAll(this));
    children.forEach(c -> c.parent = null);
    children.clear();
  }

  /**
   * Sets the tree to which this node belongs.
   *
   * @param tree the tree to set
   */
  void setTree(Tree tree) {
    this.tree = tree;

    if (tree != null) {
      // Set the tree for all children
      for (TreeNode child : children) {
        child.setTree(tree);
      }
    }
  }

  /**
   * Returns the tree to which this node belongs.
   *
   * @return an Optional containing the tree if it exists, otherwise an empty Optional
   */
  public Optional<Tree> getTree() {
    return Optional.ofNullable(tree);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String toString() {
    return text;
  }

  private void withTree(Consumer<Tree> action) {
    if (tree != null && tree.isAttached()) {
      try {
        action.accept(tree);
      } catch (Exception e) {
        throw new WebforjRuntimeException("Tree operation failed", e);
      }
    }
  }

  private AtomicInteger getIdGenerator() {
    if (Environment.isPresent()) {
      if (!ObjectTable.contains(LOOKUP_KEY)) {
        ObjectTable.put(LOOKUP_KEY, new AtomicInteger());
      }

      return (AtomicInteger) ObjectTable.get(LOOKUP_KEY);
    } else {
      return ID_GENERATOR;
    }
  }

  void destroy() {
    removeAll();
    ID_GENERATOR.set(0);
  }
}
