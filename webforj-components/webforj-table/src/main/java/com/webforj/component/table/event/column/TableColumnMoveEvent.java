package com.webforj.component.table.event.column;

import com.webforj.component.element.annotation.EventName;
import com.webforj.component.element.annotation.EventOptions;
import com.webforj.component.element.annotation.EventOptions.EventData;
import com.webforj.component.table.Table;
import java.util.Map;

/**
 * Event fired when a table column is moved to a different position.
 *
 * @author Hyyan Abo Fakher
 * @since 25.03
 */
@EventName("dwc-column-moved")
@EventOptions(data = {@EventData(key = "id", exp = "event.detail.id"),
    @EventData(key = "oldIndex", exp = "event.detail.oldIndex"),
    @EventData(key = "newIndex", exp = "event.detail.newIndex")})
public class TableColumnMoveEvent extends TableColumnEvent<Object> {
  private final int oldIndex;
  private final int newIndex;

  /**
   * Creates a new column move event.
   *
   * @param table the table
   * @param eventMap the event map
   */
  public TableColumnMoveEvent(Table<Object> table, Map<String, Object> eventMap) {
    super(table, eventMap);
    this.oldIndex = Integer.parseInt(String.valueOf(eventMap.get("oldIndex")));
    this.newIndex = Integer.parseInt(String.valueOf(eventMap.get("newIndex")));
  }

  /**
   * Gets the old index of the column.
   *
   * @return the old order index
   */
  public int getOldIndex() {
    return oldIndex;
  }

  /**
   * Gets the new index of the column.
   *
   * @return the new index
   */
  public int getNewIndex() {
    return newIndex;
  }
}
