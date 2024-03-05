package com.webforj.component.table.event.cell;

import com.webforj.component.element.annotation.EventName;
import com.webforj.component.table.Table;
import java.util.Map;

/**
 * Emitted when a cell is clicked.
 *
 * @param <T> the type of the item
 *
 * @author Hyyan Abo Fakher
 * @since 24.00
 */
@EventName("dwc-cell-clicked")
public class TableCellClickEvent<T> extends TableCellEvent<T> {

  /**
   * Creates a new cell click event.
   *
   * @param table the table
   * @param eventMap the event map
   */
  public TableCellClickEvent(Table<T> table, Map<String, Object> eventMap) {
    super(table, eventMap);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String toString() {
    return "TableCellClickEvent" + super.toString();
  }
}
