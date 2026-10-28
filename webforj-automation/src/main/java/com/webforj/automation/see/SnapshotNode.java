package com.webforj.automation.see;

import com.webforj.component.Component;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

/**
 * Represents a node of a snapshot. A node has a type, a text and attributes, and contains the nodes
 * of its child components and the content lines added by serializers.
 *
 * <p>
 * Content lines are either structure or data. Structure lines, added with
 * {@link #addChild(String)}, are always rendered, such as the columns of a table or the tabs of a
 * tabbed pane. Data lines, added with {@link #addItem(String)}, are rendered up to the maximum
 * width of the snapshot, such as the rows of a table or the items of a list.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class SnapshotNode {
  private final String type;
  private final Component component;
  private final SnapshotRenderer renderer;
  private final SnapshotNode parent;
  private final Map<String, String> attributes = new LinkedHashMap<>();
  private final Set<String> keys = new LinkedHashSet<>();
  private final Set<String> textAttributes = new HashSet<>();
  private final List<SnapshotNode> children = new ArrayList<>();
  private final List<SnapshotNode> structure = new ArrayList<>();
  private final List<SnapshotNode> items = new ArrayList<>();
  private Integer itemCount;
  private String text;

  SnapshotNode(String type, Component component, SnapshotRenderer renderer, SnapshotNode parent) {
    this.type = type;
    this.component = component;
    this.renderer = renderer;
    this.parent = parent;
  }

  /**
   * Returns the type of the node.
   *
   * @return the type
   */
  public String getType() {
    return type;
  }

  /**
   * Returns the component of the node.
   *
   * @return the component, or null if the node is a content line
   */
  public Component getComponent() {
    return component;
  }

  /**
   * Returns the parent node.
   *
   * @return the parent node, or null if the node is the root or a content line
   */
  public SnapshotNode getParent() {
    return parent;
  }

  /**
   * Returns the text of the node.
   *
   * @return the text, or null if the node has no text
   */
  public String getText() {
    return text;
  }

  /**
   * Sets the text of the node, rendered in quotes after the type. An empty text clears it.
   *
   * @param text the text
   * @return the node itself
   */
  public SnapshotNode setText(String text) {
    this.text = text == null || text.isEmpty() ? null : text;

    return this;
  }

  /**
   * Returns the value of the given attribute.
   *
   * @param key the attribute key
   * @return the value, or null if the attribute is a flag or is not set
   */
  public String getAttribute(String key) {
    return attributes.get(key);
  }

  /**
   * Checks if the given attribute or flag is set.
   *
   * @param key the attribute key
   * @return true if the attribute or flag is set
   */
  public boolean hasAttribute(String key) {
    return attributes.containsKey(key);
  }

  /**
   * Sets the given attribute, rendered as {@code [key=value]}. A null or empty value removes it. An
   * attribute renders at the place of the first write of its key, whoever writes it later.
   *
   * @param key the attribute key
   * @param value the value
   *
   * @return the node itself
   */
  public SnapshotNode setAttribute(String key, Object value) {
    Objects.requireNonNull(key, "The attribute key cannot be null");
    String printed = value == null ? null : String.valueOf(value);
    keys.add(key);
    if (printed == null || printed.isEmpty()) {
      attributes.remove(key);
    } else {
      attributes.put(key, printed);
    }

    renderer.recordWrite(this, key);

    return this;
  }

  /**
   * Removes the given attribute or flag.
   *
   * @param key the attribute key
   * @return the node itself
   */
  public SnapshotNode removeAttribute(String key) {
    attributes.remove(key);

    return this;
  }

  /**
   * Adds the given flag, rendered as {@code [key]}.
   *
   * @param key the flag key
   * @return the node itself
   */
  public SnapshotNode addFlag(String key) {
    Objects.requireNonNull(key, "The flag key cannot be null");
    keys.add(key);
    attributes.put(key, null);
    renderer.recordWrite(this, key);

    return this;
  }

  /**
   * Returns the nodes of the child components in the order they were added. The returned list is
   * live and serializers can move nodes between lists.
   *
   * @return the child nodes
   */
  public List<SnapshotNode> getChildren() {
    return children;
  }

  /**
   * Adds a structure line to the node. Structure lines are always rendered.
   *
   * @param type the type of the line
   * @return the added node
   */
  public SnapshotNode addChild(String type) {
    SnapshotNode node = new SnapshotNode(type, null, renderer, this);
    structure.add(node);

    return node;
  }

  /**
   * Adds a node for the given component. When the component is hidden and hidden components are not
   * included, the returned node is not part of the snapshot.
   *
   * @param child the component
   * @return the added node
   */
  public SnapshotNode addChild(Component child) {
    if (!renderer.isShown(child)) {
      return new SnapshotNode(child.getClass().getSimpleName(), child, renderer, this);
    }

    SnapshotNode node = renderer.describe(child);
    structure.add(node);

    return node;
  }

  /**
   * Adds a data line to the node. Data lines are rendered up to the maximum width of the snapshot.
   *
   * @param type the type of the line
   * @return the added node
   */
  public SnapshotNode addItem(String type) {
    SnapshotNode node = new SnapshotNode(type, null, renderer, this);
    items.add(node);

    return node;
  }

  /**
   * Sets the number of items the component holds, stated on the line that ends a cut list. A
   * component that cannot tell how many items it holds leaves it unset, and the line then only
   * shows that more items follow.
   *
   * @param count the number of items the component holds
   * @return the node itself
   *
   * @throws IllegalArgumentException if the count is negative
   */
  public SnapshotNode setItemCount(int count) {
    if (count < 0) {
      throw new IllegalArgumentException("The item count cannot be negative");
    }

    itemCount = count;

    return this;
  }

  /**
   * Returns the maximum number of data lines rendered under the node.
   *
   * @return the maximum width
   */
  public int getMaxWidth() {
    return renderer.getMaxWidth();
  }

  /**
   * Checks if the snapshot includes the browser state. Serializers read state held in the browser
   * only when this returns true.
   *
   * @return true if the snapshot includes the browser state
   */
  public boolean isBrowserState() {
    return renderer.isBrowserState();
  }

  boolean isComposites() {
    return renderer.isComposites();
  }

  SnapshotNode setTextAttribute(String key, Object value) {
    textAttributes.add(key);

    return setAttribute(key, value);
  }

  boolean isTextAttribute(String key) {
    return textAttributes.contains(key);
  }

  Map<String, String> getAttributes() {
    Map<String, String> ordered = new LinkedHashMap<>();
    for (String key : keys) {
      if (attributes.containsKey(key)) {
        ordered.put(key, attributes.get(key));
      }
    }

    return Collections.unmodifiableMap(ordered);
  }

  List<SnapshotNode> getStructure() {
    return structure;
  }

  List<SnapshotNode> getItems() {
    return items;
  }

  Integer getItemCount() {
    return itemCount;
  }
}
