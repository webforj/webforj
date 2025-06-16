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

  Table<Person> table = null;

  @BeforeEach
  void setup() {
    table = new Table<Person>();
    table.addColumn("name", Person::name);
    table.addColumn("city", Person::city);
    table.addColumn("age", Person::age);

    table.setItems(List.of(new Person("Hyyan", "Syria", 30), new Person("Stephen", "DE", 25),
        new Person("John", "US", 35), new Person("Jane", "UK", 28)));
  }

  @Test
  void shouldReturnSortCriteria() {
    Map<String, Object> eventMap = new HashMap<>();
    eventMap.put("criteria", """
        [
          {
            "id": "name",
            "sort": "asc",
            "sortIndex": 1
          },
          {
            "id": "city",
            "sort": "asc",
            "sortIndex": 2
          },
          {
            "id": "age",
            "sort": "desc",
            "sortIndex": 3
          }
        ]
        """);

    TableSortChangeEvent<Person> event = new TableSortChangeEvent<>(table, eventMap);

    OrderCriteriaList<Person> criterion = event.getOrderCriteriaList();
    assertEquals(3, criterion.size());

    OrderCriteria<Person, ?> criteria = criterion.iterator().next();
    assertEquals(OrderCriteria.Direction.ASC, criteria.getDirection());
  }

  static record Person(String name, String city, int age) {
    public Person(String name, String city, int age) {
      this.name = name;
      this.city = city;
      this.age = age;
    }
  }
}
