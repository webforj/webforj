package org.dwcj.addons.table.event.renderer;

import java.util.Map;
import org.dwcj.addons.table.Table;
import org.dwcj.addons.table.event.cell.TableCellEvent;

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
