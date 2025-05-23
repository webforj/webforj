package com.webforj.component.tree;

import com.basis.bbj.proxies.SysGuiProxyConstants;
import com.basis.bbj.proxies.sysgui.BBjTree;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import com.basis.startup.type.BBjVector;
import com.basis.util.common.BasisNumber;
import com.google.gson.annotations.SerializedName;
import com.webforj.Page;
import com.webforj.bridge.ComponentAccessor;
import com.webforj.bridge.WindowAccessor;
import com.webforj.component.DwcFocusableComponent;
import com.webforj.component.event.ComponentEventSinkRegistry;
import com.webforj.component.icons.IconDefinition;
import com.webforj.component.tree.event.TreeClickEvent;
import com.webforj.component.tree.event.TreeCollapseEvent;
import com.webforj.component.tree.event.TreeDeselectEvent;
import com.webforj.component.tree.event.TreeDoubleClickEvent;
import com.webforj.component.tree.event.TreeExpandEvent;
import com.webforj.component.tree.event.TreeSelectEvent;
import com.webforj.component.tree.sink.TreeClickEventSink;
import com.webforj.component.tree.sink.TreeCollapseEventSink;
import com.webforj.component.tree.sink.TreeDeselectEventSink;
import com.webforj.component.tree.sink.TreeDoubleClickEventSink;
import com.webforj.component.tree.sink.TreeExpandEventSink;
import com.webforj.component.tree.sink.TreeSelectEventSink;
import com.webforj.component.window.Window;
import com.webforj.data.selection.ItemMultiSelectable;
import com.webforj.data.selection.ItemSingleSelectable;
import com.webforj.data.selection.KeyMultiSelectable;
import com.webforj.data.selection.KeySingleSelectable;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;
import com.webforj.exceptions.WebforjRuntimeException;
import com.webforj.utilities.Assets;
import com.webforj.utilities.BBjFunctionalityHelper;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;

/**
 * A UI component for displaying and interacting with hierarchical data.
 *
 * <p>
 * The {@code Tree} component allows you to build expandable, selectable trees of nodes with support
 * for dynamic updates, icon customization, and event handling.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 25.01
 */
public class Tree extends DwcFocusableComponent<Tree>
    implements TreeNodeDelegate, ItemSingleSelectable<Tree, TreeNode>, KeySingleSelectable<Tree>,
    ItemMultiSelectable<Tree, TreeNode>, KeyMultiSelectable<Tree> {
  static final String PROP_CONNECT = "connect";
  private static final String NULL_KEY_MESSAGE = "Key cannot be null";
  private static final String NULL_TEXT_MESSAGE = "Text cannot be null";
  private static final String NULL_ITEM_MESSAGE = "Item cannot be null";
  private static final String NULL_ITEMS_MESSAGE = "Items cannot be null";
  private static final String NULL_NODE_MESSAGE = "Node cannot be null";
  private static final String NULL_MODE_MESSAGE = "Selection mode cannot be null";
  private static final String NULL_COLLAPSED_ICON_MESSAGE =
      "Icon for collapsed nodes cannot be null";
  private static final String NULL_EXPANDED_ICON_MESSAGE = "Icon for expanded nodes cannot be null";
  private static final String NULL_LEAF_ICON_MESSAGE = "Icon for leaf nodes cannot be null";
  private static final String NULL_LEAF_SELECTED_ICON_MESSAGE =
      "Icon for selected leaf nodes cannot be null";
  private static final String ICON_FORMAT = "%s:%s";

  private final String uuid = UUID.randomUUID().toString();
  private final TreeNode root = new TreeNode(this);
  private final Set<TreeNode> selectedNodes = new HashSet<>();
  private final Set<TreeNode> expandedNodes = new HashSet<>();
  private final Set<TreeNode> expandedFromNodes = new HashSet<>();
  private SelectionMode selectionMode = SelectionMode.MULTIPLE;
  private String collapsedIcon = "dwc:folder";
  private String expandedIcon = "dwc:folder-opened";
  private String leafIcon = "dwc:file";
  private String selectedLeafIcon = "dwc:file";
  private Map<TreeNode, String> nodeIcons = new HashMap<>();
  private Map<TreeNode, String> nodeSelectedIcons = new HashMap<>();
  private boolean groupIconsVisible = true;
  private boolean leafIconsVisible = true;
  private boolean connect = true;

  private final ComponentEventSinkRegistry<TreeSelectEvent> selectEventSinkListenerRegistry =
      new ComponentEventSinkRegistry<>(new TreeSelectEventSink(this, getEventDispatcher()),
          TreeSelectEvent.class);
  private final ComponentEventSinkRegistry<TreeDeselectEvent> deselectEventSinkListenerRegistry =
      new ComponentEventSinkRegistry<>(new TreeDeselectEventSink(this, getEventDispatcher()),
          TreeDeselectEvent.class);
  private final ComponentEventSinkRegistry<TreeCollapseEvent> collapseEventSinkListenerRegistry =
      new ComponentEventSinkRegistry<>(new TreeCollapseEventSink(this, getEventDispatcher()),
          TreeCollapseEvent.class);
  private final ComponentEventSinkRegistry<TreeExpandEvent> expandEventSinkListenerRegistry =
      new ComponentEventSinkRegistry<>(new TreeExpandEventSink(this, getEventDispatcher()),
          TreeExpandEvent.class);
  private final ComponentEventSinkRegistry<TreeClickEvent> clickEventSinkListenerRegistry =
      new ComponentEventSinkRegistry<>(new TreeClickEventSink(this, getEventDispatcher()),
          TreeClickEvent.class);
  private final ComponentEventSinkRegistry<TreeDoubleClickEvent> doubleClickEventSinkListenerRegistry =
      new ComponentEventSinkRegistry<>(new TreeDoubleClickEventSink(this, getEventDispatcher()),
          TreeDoubleClickEvent.class);

  /**
   * The selection mode for the tree.
   */
  public enum SelectionMode {
    /**
     * One node at a time can be selected.
     */
    @SerializedName("single")
    SINGLE,

    /**
     * Multiple rows can be selected.
     */
    @SerializedName("multiple")
    MULTIPLE
  }

  /**
   * Creates a new tree component.
   */
  public Tree() {
    super();
    setAttribute("data-ref", uuid);
  }

  /**
   * Creates a new tree node with the given key and text.
   *
   * @param key the non-null key identifying this node
   * @param text the non-null display text of this node
   *
   * @return a new TreeNode instance
   * @throws NullPointerException if {@code key} or {@code text} is null
   */
  public static TreeNode node(Object key, String text) {
    Objects.requireNonNull(key, NULL_KEY_MESSAGE);
    Objects.requireNonNull(text, NULL_TEXT_MESSAGE);
    return new TreeNode(key, text);
  }

  /**
   * Creates a new tree node whose key equals its text.
   *
   * @param text the non-null display text and key for this node
   *
   * @return a new TreeNode instance
   * @throws NullPointerException if {@code text} is null
   */
  public static TreeNode node(String text) {
    Objects.requireNonNull(text, NULL_TEXT_MESSAGE);
    return new TreeNode(text);
  }

  /**
   * Gets the root node of this tree.
   *
   * @return the root node of this tree
   */
  public TreeNode getRoot() {
    return root;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Object getKey() {
    return root.getKey();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int getUniqueId() {
    return root.getUniqueId();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Optional<TreeNode> getParent() {
    return root.getParent();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public List<TreeNode> getChildren() {
    return root.getChildren();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Tree insert(int index, TreeNode child) {
    root.insert(index, child);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public TreeNode insert(int index, String text) {
    return root.insert(index, text);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Tree insert(int index, TreeNode... nodes) {
    root.insert(index, nodes);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Tree insert(int index, String... texts) {
    root.insert(index, texts);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Tree add(TreeNode child) {
    root.add(child);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public TreeNode add(String text) {
    return root.add(text);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Tree add(TreeNode... nodes) {
    root.add(nodes);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Tree add(String... texts) {
    root.add(texts);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void remove(TreeNode child) {
    root.remove(child);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void removeAll() {
    root.removeAll();
  }

  /**
   * Sets the selection mode for the tree.
   *
   * @param mode the selection mode
   * @return the component itself
   * @throws NullPointerException if {@code mode} is null
   */
  public Tree setSelectionMode(SelectionMode mode) {
    Objects.requireNonNull(mode, NULL_MODE_MESSAGE);

    if (selectionMode == SelectionMode.MULTIPLE && mode == SelectionMode.SINGLE) {
      List<TreeNode> nodes = getSelectedItems();
      if (!nodes.isEmpty()) {
        TreeNode first = nodes.iterator().next();
        deselectAll();
        select(first);
      }
    }


    selectionMode = mode;
    doSetSelectionMode(mode);
    return this;
  }

  /**
   * Gets the current selection mode of the tree.
   *
   * @return the current selection mode
   */
  public SelectionMode getSelectionMode() {
    return selectionMode;
  }

  /**
   * {@inheritDoc}
   *
   * @throws NullPointerException if {@code item} is null
   */
  @Override
  public Tree select(TreeNode item) {
    Objects.requireNonNull(item, NULL_ITEM_MESSAGE);

    selectedNodes.add(item);
    doSelect(item);
    return this;
  }

  /**
   * {@inheritDoc}
   *
   * @throws NullPointerException if {@code items} is null
   */
  @Override
  public Tree select(TreeNode... items) {
    Objects.requireNonNull(items, NULL_ITEMS_MESSAGE);

    for (TreeNode item : items) {
      select(item);
    }

    return this;
  }

  /**
   * {@inheritDoc}
   *
   * @throws NullPointerException if {@code key} is null
   * @throws IllegalArgumentException if no node is found with the given key
   */
  @Override
  public Tree selectKey(Object key) {
    Objects.requireNonNull(key, NULL_KEY_MESSAGE);

    Optional<TreeNode> node = Tree.findByKey(root, key);
    if (!node.isPresent()) {
      throw new IllegalArgumentException("No node found with the given key");
    }

    select(node.get());
    return this;
  }

  /**
   * {@inheritDoc}
   *
   * @throws NullPointerException if {@code keys} is null
   * @throws IllegalArgumentException if no node is found with the given key
   */
  @Override
  public Tree selectKey(Object... keys) {
    Objects.requireNonNull(keys, NULL_ITEMS_MESSAGE);

    for (Object key : keys) {
      selectKey(key);
    }

    return this;
  }

  /**
   * Selects all of the children in a parent node.
   *
   * @param node the parent node whose children will be selected
   *
   * @return the component itself
   * @throws NullPointerException if {@code node} is null
   */
  public Tree selectChildren(TreeNode node) {
    Objects.requireNonNull(node, NULL_NODE_MESSAGE);

    for (TreeNode child : node.getChildren()) {
      selectedNodes.add(child);
    }

    doSelectChildren(node);

    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Tree deselect() {
    List<TreeNode> nodes = getSelectedItems();
    if (!nodes.isEmpty()) {
      TreeNode first = nodes.iterator().next();
      selectedNodes.remove(first);
      doDeselect(first);
    }

    return this;
  }

  /**
   * {@inheritDoc}
   *
   * @throws NullPointerException if {@code items} is null
   */
  @Override
  public Tree deselect(TreeNode... items) {
    Objects.requireNonNull(items, NULL_ITEMS_MESSAGE);

    for (TreeNode item : items) {
      selectedNodes.remove(item);
      doDeselect(item);
    }

    return this;
  }

  /**
   * {@inheritDoc}
   *
   * @throws NullPointerException if {@code key} is null
   */
  @Override
  public Tree deselectKey(Object... key) {
    Objects.requireNonNull(key, NULL_ITEMS_MESSAGE);

    for (Object k : key) {
      Optional<TreeNode> node = Tree.findByKey(root, k);
      if (node.isPresent()) {
        deselect(node.get());
      }
    }

    return this;
  }

  /**
   * Deselects all of the children in a parent node.
   *
   * @param node the parent node whose children will be deselected
   *
   * @return the component itself
   * @throws NullPointerException if {@code node} is null
   */
  public Tree deselectChildren(TreeNode node) {
    Objects.requireNonNull(node, NULL_NODE_MESSAGE);

    for (TreeNode child : node.getChildren()) {
      selectedNodes.remove(child);
    }

    doDeselectChildren(node);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Tree deselectAll() {
    selectedNodes.clear();
    doDeselectAll();
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public TreeNode getSelected() {
    if (!isAttached()) {
      if (selectedNodes.isEmpty()) {
        return null;
      }

      return selectedNodes.iterator().next();
    } else {
      return doGetSelected();
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public List<TreeNode> getSelectedItems() {
    if (!isAttached()) {
      return Collections.unmodifiableList(new ArrayList<>(selectedNodes));
    } else {
      return doGetSelectedItems();
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Object getSelectedKey() {
    return Optional.ofNullable(getSelected()).map(TreeNode::getKey).orElse(null);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public List<Object> getSelectedKeys() {
    return Optional.ofNullable(getSelectedItems()).map(items -> {
      List<Object> keys = new ArrayList<>();
      for (TreeNode item : items) {
        keys.add(item.getKey());
      }

      return keys;
    }).orElse(Collections.emptyList());
  }

  /**
   * Collapses the given node.
   *
   * @param node the node to collapse
   * @return the component itself
   * @throws NullPointerException if {@code node} is null
   */
  public Tree collapse(TreeNode node) {
    Objects.requireNonNull(node, NULL_NODE_MESSAGE);
    expandedNodes.remove(node);
    expandedFromNodes.remove(node);
    doCollapse(node);
    return this;
  }

  /**
   * Collapses the given node.
   *
   * @param key the key of the node to collapse
   * @return the component itself
   * @throws NullPointerException if {@code node} is null
   */
  public Tree collapse(Object key) {
    Objects.requireNonNull(key, NULL_KEY_MESSAGE);

    Optional<TreeNode> node = Tree.findByKey(root, key);
    if (node.isPresent()) {
      collapse(node.get());
    } else {
      throw new IllegalArgumentException("No node found with the given key");
    }

    return this;
  }

  /**
   * Collapses a node and all of its descendant's nodes.
   *
   * @param node the node to collapse
   * @return the component itself
   * @throws NullPointerException if {@code node} is null
   */
  public Tree collapseFrom(TreeNode node) {
    Objects.requireNonNull(node, NULL_NODE_MESSAGE);
    expandedNodes.remove(node);
    expandedFromNodes.remove(node);
    doCollapseFromNode(node);

    return this;
  }

  /**
   * Collapses a node and all of its descendant's nodes.
   *
   * @param key the key of the node to collapse
   * @return the component itself
   * @throws NullPointerException if {@code node} is null
   */
  public Tree collapseFrom(Object key) {
    Objects.requireNonNull(key, NULL_KEY_MESSAGE);

    Optional<TreeNode> node = Tree.findByKey(root, key);
    if (node.isPresent()) {
      collapseFrom(node.get());
    } else {
      throw new IllegalArgumentException("No node found with the given key");
    }

    return this;
  }

  /**
   * Expands the given node.
   *
   * @param node the node to expand
   * @return the component itself
   * @throws NullPointerException if {@code node} is null
   */
  public Tree expand(TreeNode node) {
    Objects.requireNonNull(node, NULL_NODE_MESSAGE);
    expandedNodes.add(node);
    doExpand(node);
    return this;
  }

  /**
   * Expands the given node.
   *
   * @param key the key of the node to expand
   * @return the component itself
   * @throws NullPointerException if {@code node} is null
   */
  public Tree expand(Object key) {
    Objects.requireNonNull(key, NULL_KEY_MESSAGE);

    Optional<TreeNode> node = Tree.findByKey(root, key);
    if (node.isPresent()) {
      expand(node.get());
    } else {
      throw new IllegalArgumentException("No node found with the given key");
    }

    return this;
  }

  /**
   * Expands a node and all of its descendant's nodes.
   *
   * @param node the node to expand
   * @return the component itself
   * @throws NullPointerException if {@code node} is null
   */
  public Tree expandFrom(TreeNode node) {
    Objects.requireNonNull(node, NULL_NODE_MESSAGE);
    expandedNodes.add(node);
    expandedFromNodes.add(node);
    doExpandFromNode(node);

    return this;
  }

  /**
   * Expands a node and all of its descendant's nodes.
   *
   * @param key the key of the node to expand
   * @return the component itself
   * @throws NullPointerException if {@code node} is null
   */
  public Tree expandFrom(Object key) {
    Objects.requireNonNull(key, NULL_KEY_MESSAGE);

    Optional<TreeNode> node = Tree.findByKey(root, key);
    if (node.isPresent()) {
      expandFrom(node.get());
    } else {
      throw new IllegalArgumentException("No node found with the given key");
    }

    return this;
  }

  /**
   * Checks if the given node is expanded.
   *
   * @param node the node to check
   * @return {@code true} if the node is expanded, {@code false} otherwise
   * @throws NullPointerException if {@code node} is null
   */
  public boolean isExpanded(TreeNode node) {
    Objects.requireNonNull(node, NULL_NODE_MESSAGE);

    if (!isAttached()) {
      return expandedNodes.contains(node);
    } else {
      return doIsExpanded(node);
    }
  }

  /**
   * Checks if the given node is expanded.
   *
   * @param key the key of the node to check
   * @return {@code true} if the node is expanded, {@code false} otherwise
   * @throws NullPointerException if {@code node} is null
   */
  public boolean isExpanded(Object key) {
    Objects.requireNonNull(key, NULL_KEY_MESSAGE);

    Optional<TreeNode> node = Tree.findByKey(root, key);
    if (node.isPresent()) {
      return isExpanded(node.get());
    } else {
      throw new IllegalArgumentException("No node found with the given key");
    }
  }

  /**
   * Checks if the given node is collapsed.
   *
   * @param node the node to check
   * @return {@code true} if the node is collapsed, {@code false} otherwise
   * @throws NullPointerException if {@code node} is null
   */
  public boolean isCollapsed(TreeNode node) {
    return !isExpanded(node);
  }

  /**
   * Checks if the given node is collapsed.
   *
   * @param key the key of the node to check
   * @return {@code true} if the node is collapsed, {@code false} otherwise
   * @throws NullPointerException if {@code node} is null
   */
  public boolean isCollapsed(Object key) {
    return !isExpanded(key);
  }

  /**
   * Sets the collapsed icon to use.
   *
   * @param src The URL of the image. If a URL is provided and begins with {@code context://}, it
   *        will be resolved as a context URL, pointing to the root of your application's resources
   *        folder, and the image URL will be a base64-encoded string of the image. If a URL is
   *        provided and starts with {@code ws://}, it will be resolved as a web server URL,
   *        pointing to the root of the web server, and the image URL will be a fully qualified URL.
   *        if a URL is provided and starts with {@code icons://}, it will be resolved as an icons
   *        URL.
   *
   * @return the component itself
   * @throws NullPointerException if {@code src} is null
   */
  public Tree setCollapsedIcon(String src) {
    Objects.requireNonNull(src, NULL_COLLAPSED_ICON_MESSAGE);
    collapsedIcon = Optional.ofNullable(Assets.resolveImageSource(src)).orElse(src);
    doSetCollapsedIcon();
    return this;
  }

  /**
   * Sets the collapsed icon to use.
   *
   * <p>
   * The method will only use the icon name and pool from the given icon. The icon component itself
   * will not be used.
   * </p>
   *
   * @param icon the icon
   *
   * @return the component itself
   * @throws NullPointerException if {@code icon} is null
   */
  public Tree setCollapsedIcon(IconDefinition<?> icon) {
    collapsedIcon = String.format(ICON_FORMAT, icon.getPool(), icon.getName());
    collapsedIcon = String.format(ICON_FORMAT, icon.getPool(), icon.getName());
    doSetCollapsedIcon();
    return this;
  }

  /**
   * Gets the resolved collapsed icon.
   *
   * <p>
   * The method will return the resolved icon property, which may be a base64-encoded string
   * representing the image or a fully qualified URL. if the icon is not set, it will return an
   * empty string.
   * </p>
   *
   * @return the icon
   */
  public String getCollapsedIcon() {
    return collapsedIcon;
  }

  /**
   * Sets the expanded icon to use.
   *
   * @param src The URL of the image. If a URL is provided and begins with {@code context://}, it
   *        will be resolved as a context URL, pointing to the root of your application's resources
   *        folder, and the image URL will be a base64-encoded string of the image. If a URL is
   *        provided and starts with {@code ws://}, it will be resolved as a web server URL,
   *        pointing to the root of the web server, and the image URL will be a fully qualified URL.
   *        if a URL is provided and starts with {@code icons://}, it will be resolved as an icons
   *        URL.
   *
   * @return the component itself
   * @throws NullPointerException if {@code src} is null
   */
  public Tree setExpandedIcon(String src) {
    Objects.requireNonNull(src, NULL_EXPANDED_ICON_MESSAGE);
    expandedIcon = Optional.ofNullable(Assets.resolveImageSource(src)).orElse(src);
    doSetExpandedIcon();
    return this;
  }

  /**
   * Sets the expanded icon to use.
   *
   * @param icon the icon
   *
   * @return the component itself
   * @throws NullPointerException if {@code icon} is null
   */
  public Tree setExpandedIcon(IconDefinition<?> icon) {
    expandedIcon = String.format(ICON_FORMAT, icon.getPool(), icon.getName());
    expandedIcon = String.format(ICON_FORMAT, icon.getPool(), icon.getName());
    doSetExpandedIcon();
    return this;
  }

  /**
   * Gets the resolved expanded icon.
   *
   * @return the icon
   */
  public String getExpandedIcon() {
    return expandedIcon;
  }

  /**
   * Sets the leaf icon to use.
   *
   * @param src The URL of the image.
   *
   * @return the component itself
   * @throws NullPointerException if {@code src} is null
   */
  public Tree setLeafIcon(String src) {
    Objects.requireNonNull(src, NULL_LEAF_ICON_MESSAGE);
    leafIcon = Optional.ofNullable(Assets.resolveImageSource(src)).orElse(src);
    doSetLeafIcon();
    return this;
  }

  /**
   * Sets the leaf icon to use.
   *
   * @param icon the icon
   * @return the component itself
   */
  public Tree setLeafIcon(IconDefinition<?> icon) {
    leafIcon = String.format(ICON_FORMAT, icon.getPool(), icon.getName());
    leafIcon = String.format(ICON_FORMAT, icon.getPool(), icon.getName());
    doSetLeafIcon();
    return this;
  }

  /**
   * Gets the resolved leaf icon.
   *
   * @return the icon
   */
  public String getLeafIcon() {
    return leafIcon;
  }

  /**
   * Sets the selected leaf icon to use.
   *
   * @param src The URL of the image.
   * @return the component itself
   * @throws NullPointerException if {@code src} is null
   */
  public Tree setLeafSelectedIcon(String src) {
    Objects.requireNonNull(src, NULL_LEAF_SELECTED_ICON_MESSAGE);
    selectedLeafIcon = Optional.ofNullable(Assets.resolveImageSource(src)).orElse(src);
    doSetLeafSelectedIcon();
    return this;
  }

  /**
   * Sets the selected leaf icon to use.
   *
   * @param icon the icon
   * @return the component itself
   */
  public Tree setLeafSelectedIcon(IconDefinition<?> icon) {
    selectedLeafIcon = String.format(ICON_FORMAT, icon.getPool(), icon.getName());
    selectedLeafIcon = String.format(ICON_FORMAT, icon.getPool(), icon.getName());
    doSetLeafSelectedIcon();
    return this;
  }

  /**
   * Gets the resolved selected leaf icon.
   *
   * @return the icon
   */
  public String getLeafSelectedIcon() {
    return selectedLeafIcon;
  }

  /**
   * When set to {@code true}, the tree will not show icons for group nodes.
   *
   * @param visible {@code true} to hide group icons, {@code false} to show them
   * @return the component itself
   */
  public Tree setGroupIconsVisible(boolean visible) {
    groupIconsVisible = visible;
    setUnrestrictedProperty("noGroupIcons", visible);
    return this;
  }

  /**
   * Checks if group icons are visible.
   *
   * @return {@code true} if group icons are visible, {@code false} otherwise
   */
  public boolean isGroupIconsVisible() {
    return groupIconsVisible;
  }

  /**
   * When set to {@code true}, the tree will not show icons for leaf nodes.
   *
   * @param visible {@code true} to hide leaf icons, {@code false} to show them
   * @return the component itself
   */
  public Tree setLeafIconsVisible(boolean visible) {
    leafIconsVisible = visible;
    setUnrestrictedProperty("noLeafIcons", visible);
    return this;
  }

  /**
   * Checks if leaf icons are visible.
   *
   * @return {@code true} if leaf icons are visible, {@code false} otherwise
   */
  public boolean isLeafIconsVisible() {
    return leafIconsVisible;
  }

  /**
   * When set to {@code true}, the tree will show a connection line between the nodes.
   *
   * @param connect {@code true} to show connection lines, {@code false} to hide them
   * @return the component itself
   */
  public Tree setConnected(boolean connect) {
    this.connect = connect;
    setUnrestrictedProperty(PROP_CONNECT, connect);
    return this;
  }

  /**
   * Checks if connection lines are shown between nodes.
   *
   * @return {@code true} if connection lines are shown, {@code false} otherwise
   */
  public boolean isConnected() {
    return connect;
  }

  /**
   * Finds a node by its key recursively in a given {@link TreeNode}.
   *
   * @param node the starting node to search from
   * @param searchKey the key to search for
   *
   * @return Optional containing the matching TreeNode, or empty if not found
   */
  public static Optional<TreeNode> findByKey(TreeNode node, Object searchKey) {
    if (Objects.equals(node.getKey(), searchKey)) {
      return Optional.of(node);
    }

    for (TreeNode child : node.getChildren()) {
      Optional<TreeNode> result = findByKey(child, searchKey);
      if (result.isPresent()) {
        return result;
      }
    }

    return Optional.empty();
  }

  /**
   * Finds a node by its node ID recursively in a given {@link TreeNode}.
   *
   * @param node the starting node to search from
   * @param searchId the node ID to search for
   *
   * @return Optional containing the matching TreeNode, or empty if not found
   */
  public static Optional<TreeNode> findById(TreeNode node, int searchId) {
    if (node.getUniqueId() == searchId) {
      return Optional.of(node);
    }

    for (TreeNode child : node.getChildren()) {
      Optional<TreeNode> result = findById(child, searchId);
      if (result.isPresent()) {
        return result;
      }
    }

    return Optional.empty();
  }

  /**
   * Adds a {@link TreeSelectEvent} listener for the component.
   *
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<TreeSelectEvent> addSelectListener(
      EventListener<TreeSelectEvent> listener) {
    return selectEventSinkListenerRegistry.addEventListener(listener);
  }

  /**
   * Alias for {@link #addSelectListener(EventListener)}.
   *
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<TreeSelectEvent> onSelect(EventListener<TreeSelectEvent> listener) {
    return addSelectListener(listener);
  }

  /**
   * Adds a {@link TreeDeselectEvent} listener for the component.
   *
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<TreeDeselectEvent> addDeselectListener(
      EventListener<TreeDeselectEvent> listener) {
    return deselectEventSinkListenerRegistry.addEventListener(listener);
  }

  /**
   * Alias for {@link #addDeselectListener(EventListener)}.
   *
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<TreeDeselectEvent> onDeselect(
      EventListener<TreeDeselectEvent> listener) {
    return addDeselectListener(listener);
  }

  /**
   * Adds a {@link TreeCollapseEvent} listener for the component.
   *
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<TreeCollapseEvent> addCollapseListener(
      EventListener<TreeCollapseEvent> listener) {
    return collapseEventSinkListenerRegistry.addEventListener(listener);
  }

  /**
   * Alias for {@link #addCollapseListener(EventListener)}.
   *
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<TreeCollapseEvent> onCollapse(
      EventListener<TreeCollapseEvent> listener) {
    return addCollapseListener(listener);
  }

  /**
   * Adds a {@link TreeExpandEvent} listener for the component.
   *
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<TreeExpandEvent> addExpandListener(
      EventListener<TreeExpandEvent> listener) {
    return expandEventSinkListenerRegistry.addEventListener(listener);
  }

  /**
   * Alias for {@link #addExpandListener(EventListener)}.
   *
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<TreeExpandEvent> onExpand(EventListener<TreeExpandEvent> listener) {
    return addExpandListener(listener);
  }

  /**
   * Adds a {@link TreeClickEvent} listener for the component.
   *
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<TreeClickEvent> addClickListener(
      EventListener<TreeClickEvent> listener) {
    return clickEventSinkListenerRegistry.addEventListener(listener);
  }

  /**
   * Alias for {@link #addClickListener(EventListener)}.
   *
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<TreeClickEvent> onClick(EventListener<TreeClickEvent> listener) {
    return addClickListener(listener);
  }

  /**
   * Adds a {@link TreeDoubleClickEvent} listener for the component.
   *
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<TreeDoubleClickEvent> addDoubleClickListener(
      EventListener<TreeDoubleClickEvent> listener) {
    return doubleClickEventSinkListenerRegistry.addEventListener(listener);
  }

  /**
   * Alias for {@link #addDoubleClickListener(EventListener)}.
   *
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<TreeDoubleClickEvent> onDoubleClick(
      EventListener<TreeDoubleClickEvent> listener) {
    return addDoubleClickListener(listener);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public List<String> getRestrictedProperties() {
    List<String> properties = super.getRestrictedProperties();
    properties.addAll(Arrays.asList("allowDeselectionByClick", "connect", "contiguousSelection",
        "disabled", "editable", "expanse", "iconCollapsed", "iconExpanded", "iconLeaf",
        "iconLeafSelected", "label", "multiSelection", "multiSelectionByClick", "nodata",
        "noGroupIcons", "noLeafIcons", "readonly", "searchCaseSensitive", "searchInput",
        "searchNodata", "searchPlaceholder", "searchTerm", "suppressExpandAll"));
    return properties;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void attachControlCallbacks() {
    super.attachControlCallbacks();
    selectEventSinkListenerRegistry.attach();
    deselectEventSinkListenerRegistry.attach();
    collapseEventSinkListenerRegistry.attach();
    expandEventSinkListenerRegistry.attach();
    clickEventSinkListenerRegistry.attach();
    doubleClickEventSinkListenerRegistry.attach();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void onAttach() {
    super.onAttach();
    boolean isVisible = isVisible();
    setVisible(false);

    if (!selectionMode.equals(SelectionMode.MULTIPLE)) {
      setSelectionMode(selectionMode);
    }

    if (!collapsedIcon.isEmpty()) {
      doSetCollapsedIcon();
    }

    if (!expandedIcon.isEmpty()) {
      doSetExpandedIcon();
    }

    if (!leafIcon.isEmpty()) {
      doSetLeafIcon();
    }

    if (!selectedLeafIcon.isEmpty()) {
      doSetLeafSelectedIcon();
    }

    var children = root.getChildren();
    for (int i = 0; i < children.size(); i++) {
      TreeNode node = children.get(i);
      doInsert(node, i);
    }

    if (!selectedNodes.isEmpty()) {
      select(selectedNodes.toArray(new TreeNode[0]));
    }

    if (!expandedNodes.isEmpty()) {
      expandedNodes.forEach(this::expand);
    }

    if (!expandedFromNodes.isEmpty()) {
      expandedFromNodes.forEach(this::expandFrom);
    }

    setVisible(isVisible);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void onDestroy() {
    super.onDestroy();

    expandedNodes.clear();
    expandedFromNodes.clear();
    selectedNodes.clear();
    root.destroy();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void onCreate(Window window) {
    try {
      BBjWindow w = WindowAccessor.getDefault().getBBjWindow(window);
      byte[] flags =
          BBjFunctionalityHelper.buildStandardCreationFlags(this.isVisible(), this.isEnabled());
      BBjTree tree = w.addTree(flags);
      tree.setRootVisible(false);
      tree.setRoot(root.getUniqueId(), "");
      setControl(tree);
    } catch (Exception e) {
      throw new WebforjRuntimeException("Failed to create the BBjTree Control", e);
    }
  }

  private String formatNodeErrorMessage(TreeNode node, String message) {
    return String.format("%s [Key: '%s', Id: %d, Text: '%s']", message, node.getKey(),
        node.getUniqueId(), node.getText());
  }

  BBjTree inferControl() {
    try {
      return (BBjTree) ComponentAccessor.getDefault().getControl(this);
    } catch (IllegalAccessException e) {
      throw new WebforjRuntimeException(e);
    }
  }

  void doSetText(TreeNode node) {
    if (!isAttached()) {
      return;
    }

    try {
      inferControl().setNodeText(node.getUniqueId(), node.getText());
    } catch (BBjException e) {
      throw new WebforjRuntimeException(formatNodeErrorMessage(node, "Failed to set text"), e);
    }
  }

  void doSetTooltipText(TreeNode node) {
    if (!isAttached()) {
      return;
    }

    try {
      inferControl().setToolTipText(node.getUniqueId(), node.getTooltipText());
    } catch (BBjException e) {
      throw new WebforjRuntimeException(formatNodeErrorMessage(node, "Failed to set tooltip text"),
          e);
    }
  }

  void doInsert(TreeNode node, int index) {
    if (!isAttached()) {
      return;
    }

    TreeNode parent = node.getParent().orElse(root);

    try {
      inferControl().insertNode(node.getUniqueId(), parent.getUniqueId(), node.getText(), index);
      doSetText(node);
      doSetTooltipText(node);
      doSetIcon(node);
      doSetSelectedIcon(node);

      var children = node.getChildren();
      for (int i = 0; i < children.size(); i++) {
        TreeNode child = children.get(i);
        doInsert(child, i);
      }

    } catch (BBjException e) {
      throw new WebforjRuntimeException(formatNodeErrorMessage(node, "Failed to render node"), e);
    }
  }

  void doRemove(TreeNode node) {
    if (!isAttached()) {
      return;
    }
    try {
      inferControl().removeNode(node.getUniqueId());
    } catch (BBjException e) {
      throw new WebforjRuntimeException(formatNodeErrorMessage(node, "Failed to remove node"), e);
    }
  }

  void doRemoveAll(TreeNode node) {
    if (!isAttached()) {
      return;
    }
    BBjTree tree = inferControl();
    try {
      tree.removeDescendants(node.getUniqueId());
    } catch (BBjException e) {
      throw new WebforjRuntimeException(
          formatNodeErrorMessage(node, "Failed to remove all nodes under"), e);
    }
  }

  void doSetSelectionMode(SelectionMode mode) {
    if (!isAttached()) {
      return;
    }

    inferControl().setSelectionMode(
        mode == SelectionMode.SINGLE ? SysGuiProxyConstants.SINGLE_TREE_SELECTION.intValue()
            : SysGuiProxyConstants.DISCONTIGUOUS_TREE_SELECTION.intValue());
  }

  void doSelect(TreeNode node) {
    if (!isAttached()) {
      return;
    }
    try {
      inferControl().selectNode(node.getUniqueId());
    } catch (BBjException e) {
      throw new WebforjRuntimeException(formatNodeErrorMessage(node, "Failed to select node"), e);
    }
  }

  void doDeselect(TreeNode node) {
    if (!isAttached()) {
      return;
    }

    try {
      inferControl().deselectNode(node.getUniqueId());
    } catch (BBjException e) {
      throw new WebforjRuntimeException(formatNodeErrorMessage(node, "Failed to deselect node"), e);
    }
  }

  void doSelectChildren(TreeNode node) {
    if (!isAttached()) {
      return;
    }

    try {
      inferControl().selectChildren(node.getUniqueId());
    } catch (BBjException e) {
      throw new WebforjRuntimeException(
          formatNodeErrorMessage(node, "Failed to select all children of node"), e);
    }
  }

  void doDeselectChildren(TreeNode node) {
    if (!isAttached()) {
      return;
    }

    try {
      inferControl().deselectChildren(node.getUniqueId());
    } catch (BBjException e) {
      throw new WebforjRuntimeException(
          formatNodeErrorMessage(node, "Failed to deselect all children of node"), e);
    }
  }

  void doDeselectAll() {
    if (!isAttached()) {
      return;
    }

    try {
      inferControl().deselectAll();
    } catch (BBjException e) {
      throw new WebforjRuntimeException("Failed to deselect all nodes", e);
    }
  }

  TreeNode doGetSelected() {
    if (!isAttached()) {
      return null;
    }

    try {
      int id = inferControl().getSelectedNode();
      return Tree.findById(root, id).orElse(null);
    } catch (BBjException e) {
      throw new WebforjRuntimeException("Failed to get selected node", e);
    }
  }

  @SuppressWarnings("unchecked")
  List<TreeNode> doGetSelectedItems() {
    if (!isAttached()) {
      return Collections.emptyList();
    }

    try {
      List<TreeNode> selected = new ArrayList<>();
      BBjVector ids = inferControl().getSelectedNodes();
      Iterator<Object> iterator = ids.iterator();
      while (iterator.hasNext()) {
        int id = ((BasisNumber) iterator.next()).intValue();
        TreeNode node = Tree.findById(root, id).orElse(null);
        if (node != null) {
          selected.add(node);
        }
      }

      return Collections.unmodifiableList(selected);
    } catch (BBjException e) {
      throw new WebforjRuntimeException("Failed to get selected nodes", e);
    }
  }

  void doCollapse(TreeNode node) {
    if (!isAttached()) {
      return;
    }

    try {
      inferControl().collapseNode(node.getUniqueId());
    } catch (BBjException e) {
      throw new WebforjRuntimeException(formatNodeErrorMessage(node, "Failed to collapse node"), e);
    }
  }

  void doCollapseFromNode(TreeNode node) {
    if (!isAttached()) {
      return;
    }

    try {
      inferControl().collapseTreeFromNode(node.getUniqueId());
      if (node.getUniqueId() == root.getUniqueId()) {
        // expand the root node again, if the root node is collapsed
        // then the tree will be empty and looks like it was not rendered
        // because in webforJ the root node is always hidden
        inferControl().expandNode(node.getUniqueId());
      }
    } catch (BBjException e) {
      throw new WebforjRuntimeException(
          formatNodeErrorMessage(node, "Failed to collapse all nodes under"), e);
    }
  }

  void doExpand(TreeNode node) {
    if (!isAttached()) {
      return;
    }

    try {
      inferControl().expandNode(node.getUniqueId());
    } catch (BBjException e) {
      throw new WebforjRuntimeException(formatNodeErrorMessage(node, "Failed to expand node"), e);
    }
  }

  void doExpandFromNode(TreeNode node) {
    if (!isAttached()) {
      return;
    }

    try {
      inferControl().expandTreeFromNode(node.getUniqueId());
    } catch (BBjException e) {
      throw new WebforjRuntimeException(
          formatNodeErrorMessage(node, "Failed to expand all nodes under"), e);
    }
  }

  boolean doIsExpanded(TreeNode node) {
    if (!isAttached()) {
      return false;
    }

    try {
      return inferControl().isNodeExpanded(node.getUniqueId());
    } catch (BBjException e) {
      throw new WebforjRuntimeException(
          formatNodeErrorMessage(node, "Failed to check if node is expanded"), e);
    }
  }

  void doSetCollapsedIcon() {
    setUnrestrictedProperty("iconCollapsed", collapsedIcon);
  }

  void doSetExpandedIcon() {
    setUnrestrictedProperty("iconExpanded", expandedIcon);
  }

  void doSetLeafIcon() {
    setUnrestrictedProperty("iconLeaf", leafIcon);
  }

  void doSetLeafSelectedIcon() {
    setUnrestrictedProperty("iconLeafSelected", selectedLeafIcon);
  }

  void doSetIcon(TreeNode node) {
    if (!isAttached()) {
      nodeIcons.put(node, node.getIcon());
    } else {
      syncClientNodeIcon("icon", node, node.getIcon());
    }
  }

  void doSetSelectedIcon(TreeNode node) {
    if (!isAttached()) {
      nodeSelectedIcons.put(node, node.getSelectedIcon());
    } else {
      syncClientNodeIcon("iconSelected", node, node.getSelectedIcon());
    }
  }

  @SuppressWarnings("squid:S6203")
  void syncClientNodeIcon(String prop, TreeNode node, String icon) {
    if (icon == null || icon.isEmpty()) {
      return;
    }

    Page.ifPresent(page -> {
      String script = """
          (async () => {
            await customElements.whenDefined('dwc-tree');
            await customElements.whenDefined('dwc-tree-node');
            const tree = document.querySelector('[data-ref="%s"]');
            tree.componentOnReady().then(() => {
              const node = tree.querySelector('[data-node-id="%s"]');
              if(node) {
                node['%s'] = '%s';
              }
            });
          })();
              """;
      page.executeJsVoidAsync(String.format(script, uuid, node.getUniqueId(), prop, icon));
    });
  }
}
