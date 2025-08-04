package com.webforj.component.table.event.column;

import com.webforj.component.table.Column;
import com.webforj.component.table.Table;
import com.webforj.component.table.event.TableEvent;
import java.util.Map;

/**
 * Base class for table column events.
 *
 * @param <T> the table column type
 *
 * @author Hyyan Abo Fakher
 * @since 25.03
 */
public abstract class TableColumnEvent<T> extends TableEvent<T> {
  private String id;

  /**
   * Creates a new column event.
   *
   * @param table the table
   * @param eventMap the event map
   */
  protected TableColumnEvent(Table<T> table, Map<String, Object> eventMap) {
    super(table, eventMap);
    this.id = String.valueOf(eventMap.get("id"));
  }

  /**
   * Gets the column ID.
   *
   * @return the column ID
   * @throws IllegalArgumentException if the column with the given ID does not exist
   */
  public Column<T, ?> getColumn() {
    return getComponent().getColumns().stream().filter(column -> column.getId().equals(id))
        .findFirst()
        .orElseThrow(() -> new IllegalArgumentException("Column with ID '" + id + "' not found"));
  }
}
