package com.webforj.data.repository;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Arrays;
import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class OrderCriteriaListTest {

  OrderCriteriaList<String> list;
  OrderCriteria<String, Integer> orderCriteria1;
  OrderCriteria<String, Integer> orderCriteria2;

  @BeforeEach
  void setUp() {
    list = new OrderCriteriaList<>();
    orderCriteria1 = new OrderCriteria<>(String::length, OrderCriteria.Direction.ASC);
    orderCriteria2 = new OrderCriteria<>(String::length, OrderCriteria.Direction.DESC);
  }

  @Test
  void shouldAddOrderCriteria() {
    list.add(orderCriteria1);
    assertTrue(list.has(orderCriteria1));
  }

  @Test
  void shouldRemoveOrderCriteria() {
    list.add(orderCriteria1);
    list.remove(orderCriteria1);
    assertFalse(list.has(orderCriteria1));
  }

  @Test
  void shouldCheckIfOrderCriteriaExists() {
    list.add(orderCriteria1);
    assertTrue(list.has(orderCriteria1));
    assertFalse(list.has(orderCriteria2));
  }

  @Test
  void shouldClearOrderCriteriaList() {
    list.add(orderCriteria1);
    list.add(orderCriteria2);
    list.clear();
    assertFalse(list.has(orderCriteria1));
    assertFalse(list.has(orderCriteria2));
  }

  @Test
  void shouldSetOrderCriteriaList() {
    List<OrderCriteria<String, ?>> newList = Arrays.asList(orderCriteria1, orderCriteria2);
    list.set(newList);
    assertTrue(list.has(orderCriteria1));
    assertTrue(list.has(orderCriteria2));
  }

  @Test
  void shouldIterateOverOrderCriteriaList() {
    list.add(orderCriteria1);
    list.add(orderCriteria2);
    int count = 0;
    for (OrderCriteria<String, ?> orderCriteria : list) {
      assertTrue(orderCriteria == orderCriteria1 || orderCriteria == orderCriteria2);
      count++;
    }
    assertEquals(2, count);
  }
}
