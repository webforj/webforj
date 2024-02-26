package org.dwcj.addons.table.event.selection;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import org.dwcj.addons.table.Table;
import org.dwcj.addons.table.event.TableEvent;
import org.dwcj.component.element.annotation.EventName;
import org.dwcj.component.element.annotation.EventOptions;
import org.dwcj.component.element.annotation.EventOptions.EventData;
import org.dwcj.data.selection.event.SelectEvent;

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
public class TableItemSelectionChange<T> extends TableEvent<T> implements SelectEvent<T> {
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
  @Override
  public int getSelectedIndex() {
    T item = getSelectedItem();

    return getComponent().getRepository().getIndex(item);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public T getSelectedItem() {
    List<T> items = getSelectedItems();
    return items.isEmpty() ? null : items.get(0);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public List<Integer> getSelectedIndices() {
    List<Integer> indices = new ArrayList<>();
    for (T item : getSelectedItems()) {
      indices.add(getComponent().getRepository().getIndex(item));
    }

    return indices;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String toString() {
    return "TableRowSelectionChange" + super.toString();
  }
}
