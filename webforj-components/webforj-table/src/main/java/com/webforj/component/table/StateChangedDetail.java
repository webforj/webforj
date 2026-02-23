package com.webforj.component.table;

import java.util.List;

/**
 * Represents the detail of a state change event in a table.
 *
 * @author Hyyan Abo Fakher
 * @since 25.03
 */
class StateChangedDetail {
  private String source;
  private List<ColumnState> columnStates;
  private List<ColumnGroup> columnGroups;

  /**
   * Creates a new StateChangedDetail instance.
   *
   * @param source the source of the state change
   * @param columnStates the list of column states
   */
  StateChangedDetail(String source, List<ColumnState> columnStates) {
    this(source, columnStates, null);
  }

  /**
   * Creates a new StateChangedDetail instance.
   *
   * @param source the source of the state change
   * @param columnStates the list of column states
   * @param columnGroups the list of column groups, or {@code null} if not present
   *
   * @since 25.12
   */
  StateChangedDetail(String source, List<ColumnState> columnStates,
      List<ColumnGroup> columnGroups) {
    this.source = source;
    this.columnStates = columnStates;
    this.columnGroups = columnGroups;
  }

  /**
   * Gets the source of the state change.
   *
   * @return the source of the state change
   */
  public String getSource() {
    return source;
  }

  /**
   * Gets the list of column states.
   *
   * @return the list of column states
   */
  public List<ColumnState> getColumnStates() {
    return columnStates;
  }

  /**
   * Gets the list of column groups.
   *
   * @return the list of column groups, or {@code null} if not present in the state change
   * @since 25.12
   */
  public List<ColumnGroup> getColumnGroups() {
    return columnGroups;
  }
}
