package org.dwcj.addons.table.event.cell;

import java.util.Map;
import org.dwcj.addons.table.Column;
import org.dwcj.addons.table.Table;
import org.dwcj.addons.table.event.TableEvent;
import org.dwcj.component.element.annotation.EventOptions;
import org.dwcj.component.element.annotation.EventOptions.EventData;

/**
 * Base class for all table cell events.
 *
 * @param <T> the table item type
 *
 * @author Hyyan Abo Fakher
 * @since 24.00
 */
@EventOptions(data = {@EventData(key = "itemKey", exp = "event.detail.row.id"),
    @EventData(key = "columnKey", exp = "event.detail.column.id")})
public abstract class TableCellEvent<T> extends TableEvent<T> {
  private String itemKey;
  private String columnKey;

  /**
   * Creates a new row click event.
   *
   * @param table the table
   * @param eventMap the event map
   */
  protected TableCellEvent(Table<T> table, Map<String, Object> eventMap) {
    super(table, eventMap);

    this.itemKey = String.valueOf(eventMap.get("itemKey"));
    this.columnKey = String.valueOf(eventMap.get("columnKey"));
  }

  /**
   * Returns the clicked cell's item.
   *
   * @return the clicked cell's item
   */
  public T getItem() {
    return getItemFromKey(getItemKey());
  }

  /**
   * Returns the clicked cell's column.
   *
   * @return the clicked cell's column
   */
  public Column<T, ?> getColumn() {
    return getComponent().getColumnById(getColumnKey());
  }

  /**
   * Gets the item key.
   *
   * @return the item key
   */
  public String getItemKey() {
    return itemKey;
  }

  /**
   * Gets the column key.
   *
   * @return the column key
   */
  public String getColumnKey() {
    return columnKey;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String toString() {
    return "{" + "itemKey=" + itemKey + ", columnKey=" + columnKey + '}';
  }
}
