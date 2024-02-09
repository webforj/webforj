package org.dwcj.data.repository;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.function.Function;
import org.junit.jupiter.api.Test;

class OrderCriteriaTest {

  @Test
  void shouldReturnValueProvider() {
    Function<String, Integer> valueProvider = String::length;
    OrderCriteria<String, Integer> orderCriteria =
        new OrderCriteria<>(valueProvider, OrderCriteria.Direction.ASC);

    assertEquals(valueProvider, orderCriteria.getValueProvider());
  }

  @Test
  void shouldReturnDirection() {
    Function<String, Integer> valueProvider = String::length;
    OrderCriteria<String, Integer> orderCriteria =
        new OrderCriteria<>(valueProvider, OrderCriteria.Direction.DESC);

    assertEquals(OrderCriteria.Direction.DESC, orderCriteria.getDirection());
  }
}
