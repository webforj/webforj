package com.webforj.addons.table.event;

import com.webforj.addons.table.Column;
import com.webforj.addons.table.Table;
import com.webforj.component.element.annotation.EventName;
import com.webforj.component.element.annotation.EventOptions;
import com.webforj.component.element.annotation.EventOptions.EventData;
import com.webforj.data.repository.OrderCriteria;
import com.webforj.data.repository.OrderCriteriaList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Function;

/**
 * Represents a sort change event.
 *
 * @param <T> the type of the table data
 *
 * @author Hyyan Abo Fakher
 * @since 24.00
 */
@EventName("dwc-sort-changed")
@EventOptions(data = {@EventData(key = "criteria", exp = "event.detail")})
public class TableSortChangeEvent<T> extends TableEvent<T> {
  private Map<String, String> clientCriteria = new HashMap<>();

  /**
   * Creates a new sort event.
   *
   * @param table the table
   * @param eventMap the event map
   */
  public TableSortChangeEvent(Table<T> table, Map<String, Object> eventMap) {
    super(table, eventMap);
    this.clientCriteria = (Map<String, String>) eventMap.get("criteria");
  }

  /**
   * Returns the client side sort criteria.
   *
   * @return the client side sort criteria
   */
  public Map<String, String> getClientCriterion() {
    return clientCriteria;
  }

  /**
   * Returns the OrderCriteriaList.
   *
   * @return the OrderCriteriaList
   */
  public OrderCriteriaList<T> getOrderCriteriaList() {
    OrderCriteriaList<T> criterion = new OrderCriteriaList<>();

    // loop over the criteria and create a list of OrderCriteria
    for (Map.Entry<String, String> entry : clientCriteria.entrySet()) {
      String columnId = entry.getKey();
      OrderCriteria.Direction direction =
          entry.getValue().equalsIgnoreCase("asc") ? OrderCriteria.Direction.ASC
              : OrderCriteria.Direction.DESC;

      Column<T, ?> column = getComponent().getColumnById(columnId);
      Function<T, ?> valueProvider = column.getValueProvider();
      Comparator<T> comparator = column.getComparator();

      // create the OrderCriteria
      OrderCriteria<T, ?> serverCriteria =
          new OrderCriteria<>(valueProvider, direction, comparator);
      criterion.add(serverCriteria);
    }

    return criterion;
  }
}
