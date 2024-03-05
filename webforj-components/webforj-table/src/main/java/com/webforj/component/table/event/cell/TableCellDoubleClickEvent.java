package com.webforj.component.table.event.cell;

import com.webforj.component.element.annotation.EventName;
import com.webforj.component.table.Table;
import java.util.Map;

/**
 * Emitted when a cell is double clicked.
 *
 * @param <T> the type of the item
 *
 * @author Hyyan Abo Fakher
 * @since 24.00
 */
@EventName("dwc-cell-clicked")
public class TableCellDoubleClickEvent<T> extends TableCellEvent<T> {

  /**
   * Creates a new cell double click event.
   *
   * @param table the table
   * @param eventMap the event map
   */
  public TableCellDoubleClickEvent(Table<T> table, Map<String, Object> eventMap) {
    super(table, eventMap);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String toString() {
    return "TableCellDoubleClickEvent" + super.toString();
  }
}
