package com.webforj.component.table;

import com.webforj.component.table.Column.PinDirection;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * Represents a column group in a table, allowing columns to be visually grouped under shared
 * headers.
 *
 * <p>
 * Column groups support nesting, so a group can contain both column references and other groups.
 * </p>
 *
 * <p>
 * Use the {@link #of(String)} factory method to create a new group, then chain {@link #add} calls
 * to populate it:
 * </p>
 *
 * <pre>{@code
 * ColumnGroup.of("personal", "Personal Info").add(nameColumn).add(ageColumn);
 *
 * ColumnGroup.of("identity", "Identity").setPinDirection(Column.PinDirection.LEFT).add(nameColumn)
 *     .add(ColumnGroup.of("bio", "Bio").add(ageColumn).add(genderColumn));
 * }</pre>
 *
 * @author Hyyan Abo Fakher
 * @since 25.12
 */
public final class ColumnGroup {

  private String id;
  private String label;
  private PinDirection pinned;
  private List<ColumnGroup> children;

  ColumnGroup() {
    // Gson no-arg constructor
  }

  private ColumnGroup(String id) {
    this.id = Objects.requireNonNull(id, "Group id cannot be null");
    this.children = new ArrayList<>();
  }

  /**
   * Creates a new column group with the given id.
   *
   * @param id a unique identifier for the group
   * @return a new ColumnGroup instance
   */
  public static ColumnGroup of(String id) {
    return new ColumnGroup(id);
  }

  /**
   * Creates a new column group with the given id and label.
   *
   * @param id a unique identifier for the group
   * @param label the display label for the group header
   *
   * @return a new ColumnGroup instance
   */
  public static ColumnGroup of(String id, String label) {
    return new ColumnGroup(id).setLabel(label);
  }

  /**
   * Gets the unique identifier for this group.
   *
   * @return the group id
   */
  public String getId() {
    return id;
  }

  /**
   * Sets the display label for this group header.
   *
   * @param label the label text to display in the group header
   * @return this group for chaining
   */
  public ColumnGroup setLabel(String label) {
    this.label = label;
    return this;
  }

  /**
   * Gets the display label for this group header. If no label has been explicitly set, the group's
   * id is returned as the default label.
   *
   * @return the label, or the group id if no label was set
   */
  public String getLabel() {
    return label != null ? label : id;
  }

  /**
   * Sets the pin direction for this group. Children inherit the group's pin direction.
   *
   * @param direction the pin direction
   * @return this group for chaining
   */
  public ColumnGroup setPinDirection(PinDirection direction) {
    this.pinned = direction;
    return this;
  }

  /**
   * Gets the pin direction for this group.
   *
   * @return the pin direction, or {@code null} if not set
   */
  public PinDirection getPinDirection() {
    return pinned;
  }

  /**
   * Adds a column to this group by its ID.
   *
   * @param columnId the ID of the column to include
   * @return this group for chaining
   */
  public ColumnGroup add(String columnId) {
    Objects.requireNonNull(columnId, "Column id cannot be null");
    ColumnGroup leaf = new ColumnGroup();
    leaf.id = columnId;
    children.add(leaf);

    return this;
  }

  /**
   * Adds a column to this group.
   *
   * @param column the column to include
   * @return this group for chaining
   */
  public ColumnGroup add(Column<?, ?> column) {
    Objects.requireNonNull(column, "Column cannot be null");
    return add(column.getId());
  }

  /**
   * Adds a nested sub-group to this group.
   *
   * @param group the sub-group to nest within this group
   * @return this group for chaining
   */
  public ColumnGroup add(ColumnGroup group) {
    Objects.requireNonNull(group, "Group cannot be null");
    children.add(group);
    return this;
  }

  /**
   * Gets the children of this group (column references and nested sub-groups).
   *
   * @return an unmodifiable list of children; empty if this is a leaf column reference
   */
  public List<ColumnGroup> getChildren() {
    return children != null ? Collections.unmodifiableList(children) : Collections.emptyList();
  }

  /**
   * Finds the parent group that directly contains the given column id as a leaf child.
   *
   * @param columnId the column id to search for
   * @param groups the list of groups to search
   *
   * @return the parent group, or {@code null} if the column is not in any group
   */
  static ColumnGroup findParent(String columnId, List<ColumnGroup> groups) {
    for (ColumnGroup entry : groups) {
      ColumnGroup result = findParentIn(columnId, entry);

      if (result != null) {
        return result;
      }
    }

    return null;
  }

  /**
   * Reconciles incoming column groups with existing ones, reusing existing instances where possible
   * and updating their properties and children order from the incoming state.
   *
   * @param existing the current list of column groups, may be {@code null} or empty
   * @param incoming the incoming list of column groups from the client
   *
   * @return the reconciled list with existing instances preserved
   */
  static List<ColumnGroup> reconcile(List<ColumnGroup> existing, List<ColumnGroup> incoming) {
    if (existing == null || existing.isEmpty()) {
      return incoming;
    }

    Map<String, ColumnGroup> existingById = new HashMap<>();
    for (ColumnGroup group : existing) {
      group.collectGroups(existingById);
    }

    return reconcileList(incoming, existingById);
  }

  void collectLeafColumnIds(List<String> result) {
    if (children == null || children.isEmpty()) {
      if (id != null) {
        result.add(id);
      }
      return;
    }

    for (ColumnGroup child : children) {
      child.collectLeafColumnIds(result);
    }
  }

  private static ColumnGroup findParentIn(String columnId, ColumnGroup entry) {
    if (entry.children == null || entry.children.isEmpty()) {
      return null;
    }

    for (ColumnGroup child : entry.children) {
      if (isLeaf(child) && child.id.equals(columnId)) {
        return entry;
      }

      ColumnGroup found = findParentIn(columnId, child);
      if (found != null) {
        return found;
      }
    }
    return null;
  }

  private static List<ColumnGroup> reconcileList(List<ColumnGroup> incoming,
      Map<String, ColumnGroup> existingById) {

    List<ColumnGroup> result = new ArrayList<>();
    for (ColumnGroup incomingGroup : incoming) {
      if (incomingGroup.children != null) {
        ColumnGroup match = existingById.get(incomingGroup.id);
        if (match != null) {
          match.reconcileFrom(incomingGroup, existingById);
          result.add(match);
        } else {
          result.add(incomingGroup);
        }
      } else {
        result.add(incomingGroup);
      }
    }

    return result;
  }

  private void reconcileFrom(ColumnGroup incoming, Map<String, ColumnGroup> existingById) {
    this.label = incoming.label;
    this.pinned = incoming.pinned;
    this.children =
        reconcileList(incoming.children != null ? incoming.children : List.of(), existingById);
  }

  private void collectGroups(Map<String, ColumnGroup> map) {
    if (children != null) {
      map.put(id, this);

      for (ColumnGroup child : children) {
        child.collectGroups(map);
      }
    }
  }

  private static boolean isLeaf(ColumnGroup group) {
    return group.children == null || group.children.isEmpty();
  }
}
