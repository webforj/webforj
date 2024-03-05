package com.webforj.component.table.event.item;

import com.webforj.component.element.annotation.EventName;
import com.webforj.component.table.Table;
import java.util.Map;

/**
 * Emitted when a row is double clicked.
 *
 * @param <T> the table item type
 *
 * @author Hyyan Abo Fakher
 * @since 24.00
 */
@EventName("dwc-row-dbclicked")
public class TableItemDoubleClickEvent<T> extends TableItemEvent<T> {

  /**
   * Creates a new row click event.
   *
   * @param table the table
   * @param eventMap the event map
   */
  public TableItemDoubleClickEvent(Table<T> table, Map<String, Object> eventMap) {
    super(table, eventMap);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String toString() {
    return "TableItemDoubleClickEvent" + super.toString();
  }
}
