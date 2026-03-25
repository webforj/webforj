package com.webforj.component.table.event.selection;

import com.webforj.component.element.annotation.EventName;
import com.webforj.component.element.annotation.EventOptions;
import com.webforj.component.element.annotation.EventOptions.EventData;
import com.webforj.component.table.Table;
import com.webforj.component.table.event.TableEvent;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * Emitted when the rows selection changes.
 *
 * @param <T> the table item type
 *
 * @author Hyyan Abo Fakher
 * @since 24.00
 */
@EventName("dwc-selection-changed")
@EventOptions(data = {@EventData(key = "keys", exp = "event.target.selected")})
public class TableItemSelectionChange<T> extends TableEvent<T> {
  private List<String> keys;

  /**
   * Creates a new row event.
   *
   * @param table the table
   * @param eventMap the event map
   */
  public TableItemSelectionChange(Table<T> table, Map<String, Object> eventMap) {
    super(table, eventMap);
    this.keys = (List<String>) eventMap.get("keys");
  }

  /**
   * {@inheritDoc}
   */
  public List<T> getSelectedItems() {
    List<T> items = new ArrayList<>();
    for (String id : keys) {
      items.add(getItemFromKey(id));
    }

    return items;
  }

  /**
   * {@inheritDoc}
   */
  public T getSelectedItem() {
    List<T> items = getSelectedItems();
    return items.isEmpty() ? null : items.get(0);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String toString() {
    return "TableRowSelectionChange" + super.toString();
  }
}
