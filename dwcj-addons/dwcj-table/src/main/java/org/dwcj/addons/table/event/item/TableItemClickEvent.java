package org.dwcj.addons.table.event.item;

import java.util.Map;
import org.dwcj.addons.table.Table;
import org.dwcj.component.element.annotation.EventName;

/**
 * Emitted when a row is clicked.
 *
 * @param <T> the table item type
 *
 * @author Hyyan Abo Fakher
 * @since 24.00
 */
@EventName("bbj-row-clicked")
public class TableItemClickEvent<T> extends TableItemEvent<T> {

  /**
   * Creates a new row click event.
   *
   * @param table the table
   * @param eventMap the event map
   */
  public TableItemClickEvent(Table<T> table, Map<String, Object> eventMap) {
    super(table, eventMap);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String toString() {
    return "TableItemClickEvent" + super.toString();
  }
}
