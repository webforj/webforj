package com.webforj.component.table.event;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.webforj.component.table.Table;
import com.webforj.data.repository.OrderCriteria;
import com.webforj.data.repository.OrderCriteriaList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class TableSortChangeEventTest {

  Table<String> table = null;

  @BeforeEach
  void setup() {
    table = new Table<String>();
    table.addColumn("item", String::valueOf).setLabel("Items");
    table.setItems(List.of("Item 0"));
  }

  @Test
  void shouldReturnSortCriteria() {
    Map<String, Object> eventMap = new HashMap<>();
    eventMap.put("criteria", Map.of("item", "desc"));

    TableSortChangeEvent<String> event = new TableSortChangeEvent<>(table, eventMap);

    OrderCriteriaList<String> criterion = event.getOrderCriteriaList();
    assertEquals(1, criterion.size());

    OrderCriteria<String, ?> criteria = criterion.iterator().next();
    assertEquals(OrderCriteria.Direction.DESC, criteria.getDirection());
  }
}
