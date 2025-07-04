package com.webforj.data.repository;


import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.util.function.Predicate;
import org.junit.jupiter.api.Test;

class RepositoryCriteriaTest {

  @Test
  void shouldCreateQueryWithAllParameters() {
    Predicate<String> filter = s -> s.startsWith("A");
    var orderCriteria =
        new OrderCriteriaList<>(new OrderCriteria<>(String::length, OrderCriteria.Direction.DESC));

    RepositoryCriteria<String, Predicate<String>> query =
        new RepositoryCriteria<>(10, 20, orderCriteria, filter);

    assertEquals(10, query.getOffset());
    assertEquals(20, query.getLimit());
    assertEquals(filter, query.getFilter());
    assertEquals(1, query.getOrderCriteria().size());
  }

  @Test
  void shouldCreateQueryWithFilterOnly() {
    Predicate<String> filter = s -> s.length() > 5;
    RepositoryCriteria<String, Predicate<String>> query = new RepositoryCriteria<>(filter);

    assertEquals(0, query.getOffset());
    assertEquals(Integer.MAX_VALUE, query.getLimit());
    assertEquals(filter, query.getFilter());
    assertEquals(0, query.getOrderCriteria().size());
  }

  @Test
  void shouldCreateQueryWithPaginationOnly() {
    RepositoryCriteria<String, Predicate<String>> query = new RepositoryCriteria<>(50, 10);

    assertEquals(50, query.getOffset());
    assertEquals(10, query.getLimit());
    assertNull(query.getFilter());
    assertEquals(0, query.getOrderCriteria().size());
  }

  @Test
  void shouldCreateQueryWithFilterAndPagination() {
    Predicate<Integer> filter = i -> i % 2 == 0;
    RepositoryCriteria<Integer, Predicate<Integer>> query = new RepositoryCriteria<>(0, 5, filter);

    assertEquals(0, query.getOffset());
    assertEquals(5, query.getLimit());
    assertEquals(filter, query.getFilter());
    assertEquals(0, query.getOrderCriteria().size());
  }

  @Test
  void shouldThrowExceptionForNegativeOffset() {
    assertThrows(IllegalArgumentException.class,
        () -> new RepositoryCriteria<>(-1, 10, null, null));
  }

  @Test
  void shouldThrowExceptionForNegativeLimit() {
    assertThrows(IllegalArgumentException.class, () -> new RepositoryCriteria<>(0, -1, null, null));
  }

  @Test
  void shouldHandleNullOrderCriteria() {
    RepositoryCriteria<String, Predicate<String>> query =
        new RepositoryCriteria<>(0, 10, null, null);

    assertEquals(0, query.getOrderCriteria().size());
    assertNotNull(query.getOrderCriteria());
  }

  @Test
  void shouldImplementEqualsAndHashCode() {
    Predicate<String> filter = s -> s.isEmpty();
    var orderCriteria =
        new OrderCriteriaList<>(new OrderCriteria<>(String::length, OrderCriteria.Direction.ASC));

    RepositoryCriteria<String, Predicate<String>> query1 =
        new RepositoryCriteria<>(10, 20, orderCriteria, filter);
    RepositoryCriteria<String, Predicate<String>> query2 =
        new RepositoryCriteria<>(10, 20, orderCriteria, filter);
    RepositoryCriteria<String, Predicate<String>> query3 =
        new RepositoryCriteria<>(15, 20, orderCriteria, filter);

    assertEquals(query1, query2);
    assertEquals(query1.hashCode(), query2.hashCode());
    assertNotEquals(query1, query3);
  }
}
