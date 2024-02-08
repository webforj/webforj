package org.dwcj.data.repository;


import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import java.util.Comparator;
import java.util.function.Predicate;
import org.junit.jupiter.api.Test;


class RetrievalBuilderTest {

  @Test
  void shouldCreateDefaultCriteria() {
    RetrievalBuilder<String> criteria = new RetrievalBuilder<>();

    assertEquals(0, criteria.getOffset());
    assertEquals(Integer.MAX_VALUE, criteria.getLimit());
    assertNull(criteria.getOrderBy());
    assertNotNull(criteria.getFilter());
  }

  @Test
  void shouldCreateCriteriaWithOffsetAndLimit() {
    RetrievalBuilder<String> criteria = new RetrievalBuilder<>(10, 20);

    assertEquals(10, criteria.getOffset());
    assertEquals(20, criteria.getLimit());
    assertNull(criteria.getOrderBy());
    assertNotNull(criteria.getFilter());
  }

  @Test
  void shouldCreateCriteriaWithFilter() {
    Predicate<String> filter = s -> s.startsWith("Test");
    RetrievalBuilder<String> criteria = new RetrievalBuilder<>(filter);

    assertEquals(0, criteria.getOffset());
    assertEquals(Integer.MAX_VALUE, criteria.getLimit());
    assertNull(criteria.getOrderBy());
    assertEquals(filter, criteria.getFilter());
  }

  @Test
  void shouldCreateCriteriaWithOrderBy() {
    Comparator<String> orderBy = Comparator.naturalOrder();
    RetrievalBuilder<String> criteria = new RetrievalBuilder<>(orderBy);

    assertEquals(0, criteria.getOffset());
    assertEquals(Integer.MAX_VALUE, criteria.getLimit());
    assertEquals(orderBy, criteria.getOrderBy());
    assertNotNull(criteria.getFilter());
  }

  @Test
  void shouldCreateCriteriaWithAllParameters() {
    Predicate<String> filter = s -> s.startsWith("Test");
    Comparator<String> orderBy = Comparator.naturalOrder();
    RetrievalBuilder<String> criteria = new RetrievalBuilder<>(10, 20, orderBy, filter);

    assertEquals(10, criteria.getOffset());
    assertEquals(20, criteria.getLimit());
    assertEquals(orderBy, criteria.getOrderBy());
    assertEquals(filter, criteria.getFilter());
  }

  @Test
  void shouldSetAndGetProperties() {
    Predicate<String> filter = s -> s.startsWith("Test");
    Comparator<String> orderBy = Comparator.naturalOrder();

    RetrievalBuilder<String> criteria = new RetrievalBuilder<>();
    criteria.setOffset(10);
    criteria.setLimit(20);
    criteria.setOrderBy(orderBy);
    criteria.setFilter(filter);

    assertEquals(10, criteria.getOffset());
    assertEquals(20, criteria.getLimit());
    assertEquals(orderBy, criteria.getOrderBy());
    assertEquals(filter, criteria.getFilter());
  }
}
