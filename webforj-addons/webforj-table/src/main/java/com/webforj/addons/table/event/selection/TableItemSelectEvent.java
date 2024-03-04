package com.webforj.addons.table.event.selection;

import com.webforj.addons.table.Table;
import com.webforj.addons.table.event.item.TableItemEvent;
import com.webforj.component.element.annotation.EventName;
import java.util.Map;

/**
 * Emitted when a item is selected.
 *
 * <p>
 * Please note that this event is not triggered when multiple selection mode is active and the
 * selection is made via the header checkbox. In such cases, the {@link TableItemSelectionChange}
 * should be used instead.
 * </p>
 *
 * @param <T> the table item type
 *
 * @see TableItemSelectionChange
 * @see TableItemDeselectEvent
 *
 * @author Hyyan Abo Fakher
 * @since 24.00
 */
@EventName("dwc-row-selected")
public class TableItemSelectEvent<T> extends TableItemEvent<T> {

  /**
   * Creates a new row select event.
   *
   * @param table the table
   * @param eventMap the event map
   */
  public TableItemSelectEvent(Table<T> table, Map<String, Object> eventMap) {
    super(table, eventMap);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String toString() {
    return "TableItemSelectEvent" + super.toString();
  }
}
