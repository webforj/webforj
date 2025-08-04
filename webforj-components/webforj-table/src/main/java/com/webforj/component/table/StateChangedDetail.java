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

  /**
   * Creates a new StateChangedDetail instance.
   *
   * @param source the source of the state change
   * @param columnStates the list of column states
   */
  StateChangedDetail(String source, List<ColumnState> columnStates) {
    this.source = source;
    this.columnStates = columnStates;
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
}
