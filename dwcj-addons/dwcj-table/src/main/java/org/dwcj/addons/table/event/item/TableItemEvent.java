package org.dwcj.addons.table.event.item;

import java.util.Map;
import org.dwcj.addons.table.Table;
import org.dwcj.addons.table.event.TableEvent;
import org.dwcj.component.element.annotation.EventOptions;
import org.dwcj.component.element.annotation.EventOptions.EventData;

/**
 * Base class for table item events.
 *
 * @param <T> the table item type
 *
 * @author Hyyan Abo Fakher
 * @since 24.00
 */
@EventOptions(data = {@EventData(key = "key", exp = "event.detail.id"),
    @EventData(key = "index", exp = "event.detail.index")})
public abstract class TableItemEvent<T> extends TableEvent<T> {
  private String key;
  private int index;

  /**
   * Creates a new row event.
   *
   * @param table the table
   * @param eventMap the event map
   */
  protected TableItemEvent(Table<T> table, Map<String, Object> eventMap) {
    super(table, eventMap);

    this.key = String.valueOf(eventMap.get("key"));
    this.index = Integer.parseInt(String.valueOf(eventMap.get("index")));
  }

  /**
   * Returns the clicked item.
   *
   * @return the clicked item
   */
  public T getItem() {
    return getItemFromKey(getItemKey());
  }

  /**
   * Gets the row key.
   *
   * @return the row key
   */
  public String getItemKey() {
    return key;
  }

  /**
   * Gets the index.
   *
   * @return the index
   */
  public int getItemIndex() {
    return index;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String toString() {
    return "{" + "key='" + key + '\'' + ", index='" + index + '\'' + '}';
  }
}
