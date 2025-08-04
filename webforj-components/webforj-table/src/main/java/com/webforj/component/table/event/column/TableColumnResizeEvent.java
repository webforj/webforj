package com.webforj.component.table.event.column;

import com.webforj.component.element.annotation.EventName;
import com.webforj.component.element.annotation.EventOptions;
import com.webforj.component.element.annotation.EventOptions.EventData;
import com.webforj.component.table.Table;
import java.util.Map;

/**
 * Event fired when a table column is resized.
 *
 * @author Hyyan Abo Fakher
 * @since 25.03
 */
@EventName("dwc-column-resized")
@EventOptions(data = {@EventData(key = "id", exp = "event.detail.id"),
    @EventData(key = "oldWidth", exp = "event.detail.oldWidth"),
    @EventData(key = "newWidth", exp = "event.detail.newWidth")})
public class TableColumnResizeEvent extends TableColumnEvent<Object> {
  private final double oldWidth;
  private final double newWidth;

  /**
   * Creates a new column resize event.
   *
   * @param table the table
   * @param eventMap the event map
   */
  public TableColumnResizeEvent(Table<Object> table, Map<String, Object> eventMap) {
    super(table, eventMap);
    this.oldWidth = Integer.parseInt(String.valueOf(eventMap.get("oldWidth")));
    this.newWidth = Integer.parseInt(String.valueOf(eventMap.get("newWidth")));
  }

  /**
   * Gets the old width of the column.
   *
   * @return the old width
   */
  public int getOldWidth() {
    return oldWidth;
  }

  /**
   * Gets the new width of the column.
   *
   * @return the new width
   */
  public int getNewWidth() {
    return newWidth;
  }
}
