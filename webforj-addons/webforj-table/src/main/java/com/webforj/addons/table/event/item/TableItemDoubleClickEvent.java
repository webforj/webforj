package com.webforj.addons.table.event.item;

import com.webforj.addons.table.Table;
import com.webforj.component.element.annotation.EventName;
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
