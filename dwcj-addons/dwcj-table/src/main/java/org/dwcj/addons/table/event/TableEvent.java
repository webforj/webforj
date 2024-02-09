package org.dwcj.addons.table.event;

import java.util.Map;
import org.dwcj.addons.table.Table;
import org.dwcj.component.event.ComponentEvent;
import org.dwcj.data.EntityKeysRegistry;

/**
 * Base class for all table events.
 *
 * @param <T> the table item type
 *
 * @author Hyyan Abo Fakher
 * @since 24.00
 */
public abstract class TableEvent<T> extends ComponentEvent<Table<T>> {
  private transient Table<T> table;

  /**
   * Create a new table event.
   *
   * @param table the table instance
   * @param eventMap the event map
   */
  protected TableEvent(Table<T> table, Map<String, Object> eventMap) {
    super(table, eventMap);
    this.table = table;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Table<T> getComponent() {
    return table;
  }

  /**
   * Returns the table item from the given key.
   *
   * @param key the item key
   * @return the table item from the given key
   */
  protected T getItemFromKey(String key) {
    try {
      EntityKeysRegistry registry = table.getItemKeysRegistry();
      T item = (T) registry.getEntity(key);
      if (table.getRepository().has(item)) {
        return item;
      }
    } catch (ClassCastException e) {
      throw new IllegalStateException("The client item '" + key + "' is not a supported entity", e);
    }

    return null;
  }
}
