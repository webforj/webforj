package com.webforj.component.table.event;

import com.google.common.reflect.TypeToken;
import com.google.gson.Gson;
import com.webforj.component.element.annotation.EventName;
import com.webforj.component.element.annotation.EventOptions;
import com.webforj.component.element.annotation.EventOptions.EventData;
import com.webforj.component.table.Column;
import com.webforj.component.table.Table;
import com.webforj.data.repository.OrderCriteria;
import com.webforj.data.repository.OrderCriteriaList;
import java.util.Collections;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.List;
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
@EventOptions(data = {@EventData(key = "criteria", exp = "JSON.stringify(event.detail)")})
public class TableSortChangeEvent<T> extends TableEvent<T> {
  private final transient Map<String, String> clientCriteria = new LinkedHashMap<>();
  private final transient List<ClientCriteria> criteriaDetails;

  /**
   * Creates a new sort event.
   *
   * @param table the table
   * @param eventMap the event map
   */
  public TableSortChangeEvent(Table<T> table, Map<String, Object> eventMap) {
    super(table, eventMap);
    TypeToken<List<ClientCriteria>> criteriaListToken = new TypeToken<List<ClientCriteria>>() {};
    this.criteriaDetails =
        new Gson().fromJson((String) eventMap.get("criteria"), criteriaListToken.getType());
  }

  /**
   * Returns the client side sort criteria as a map.
   *
   * <p>
   * This method is used to get the client side sort criteria. The criteria are stored in a map
   * where the key is the column id and the value is the sort direction (asc or desc).
   * </p>
   *
   * @return the client side sort criteria
   */
  public Map<String, String> getClientCriterion() {
    // no need to sort the criteria, it is already sorted by the client
    if (clientCriteria.isEmpty()) {
      for (ClientCriteria clientCriterion : criteriaDetails) {
        String columnId = clientCriterion.getId();
        String sort = clientCriterion.getSort();
        clientCriteria.put(columnId, sort);
      }
    }

    return Collections.unmodifiableMap(clientCriteria);
  }

  /**
   * Returns the OrderCriteriaList.
   *
   * @return the OrderCriteriaList
   */
  public OrderCriteriaList<T> getOrderCriteriaList() {
    OrderCriteriaList<T> criterion = new OrderCriteriaList<>();

    // loop over the criteria and create a list of OrderCriteria
    var localClientCriterion = getClientCriterion();
    for (var entry : localClientCriterion.entrySet()) {
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

  /**
   * Represents the client criteria.
   *
   * @return the OrderCriteriaList
   * @since 25.00
   */
  static final class ClientCriteria {
    private String id;
    private String sort;
    private int sortIndex;

    /**
     * Get the id of the column.
     *
     * @return the id of the column
     */
    public String getId() {
      return id;
    }

    /**
     * Get the sort direction.
     *
     * @return the sort direction
     */
    public String getSort() {
      return sort;
    }

    /**
     * Get the sort index.
     *
     * @return the sort index
     */
    public int getSortIndex() {
      return sortIndex;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String toString() {
      return "ClientCriteria{" + "id='" + id + '\'' + ", sort='" + sort + '\'' + ", sortIndex="
          + sortIndex + '}';
    }
  }
}
