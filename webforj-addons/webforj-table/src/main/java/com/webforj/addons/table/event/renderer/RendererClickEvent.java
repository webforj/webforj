package com.webforj.addons.table.event.renderer;

import com.webforj.addons.table.Table;
import com.webforj.addons.table.event.cell.TableCellEvent;
import java.util.Map;

/**
 * Represents a renderer click event.
 *
 * @param <T> the type of the row data
 *
 * @author Hyyan Abo Fakher
 * @since 24.00
 */
public class RendererClickEvent<T> extends TableCellEvent<T> {

  /**
   * Creates a new renderer click event.
   *
   * @param table the table
   * @param eventMap the event map
   */
  public RendererClickEvent(Table<T> table, Map<String, Object> eventMap) {
    super(table, eventMap);
  }
}
