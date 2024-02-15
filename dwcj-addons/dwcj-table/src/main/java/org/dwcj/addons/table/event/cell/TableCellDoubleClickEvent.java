package org.dwcj.addons.table.event.cell;

import java.util.Map;
import org.dwcj.addons.table.Table;
import org.dwcj.component.element.annotation.EventName;

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
