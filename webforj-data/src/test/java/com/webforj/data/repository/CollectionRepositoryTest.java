package com.webforj.data.repository;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertIterableEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.data.repository.event.RepositoryCommitEvent;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.function.Predicate;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class CollectionRepositoryTest {
  private CollectionRepository<String> repository;
  private List<String> items;

  @BeforeEach
  void setUp() {
    items = new ArrayList<>(Arrays.asList("item1", "item2", "item3"));
    repository = new CollectionRepository<>(items);
  }

  @Test
  void shouldFindExistingItemByKey() {
    Optional<String> result = repository.findByKey("item1");
    assertTrue(result.isPresent());
    assertEquals("item1", result.get());
  }

  @Test
  void shouldNotFindNonExistentItem() {
    Optional<String> result = repository.findByKey("item4");
    assertTrue(result.isEmpty());
  }

  @Test
  void shouldFindAllByDefaultCriteria() {
    repository.setOffset(1);
    repository.setLimit(2);
    repository.getOrderCriteriaList()
        .add(new OrderCriteria<String, String>(s -> s, OrderCriteria.Direction.DESC));
    repository.setBaseFilter(item -> item.startsWith("item"));
    List<String> result = repository.findAll().toList();

    assertIterableEquals(Arrays.asList("item2", "item1"), result);
  }

  @Test
  void shouldFireCommitEvent() {
    repository.onCommit(event -> {
      assertEquals(RepositoryCommitEvent.class, event.getClass());
      assertEquals(event.getRepository(), repository);
      assertIterableEquals(event.getCommits(), items);
    });

    repository.commit();
  }

  @Test
  void shouldFindByRepositoryQuery() {
    RepositoryCriteria<String, Predicate<String>> query = new RepositoryCriteria<>(1, // offset
        2, // limit
        new OrderCriteriaList<>(
            new OrderCriteria<String, String>(s -> s, OrderCriteria.Direction.DESC)),
        item -> item.startsWith("item"));

    List<String> result = repository.findBy(query).toList();

    assertIterableEquals(Arrays.asList("item2", "item1"), result);
  }

  @Test
  void shouldFindByRepositoryQueryWithFilterOnly() {
    RepositoryCriteria<String, Predicate<String>> query =
        new RepositoryCriteria<>(item -> item.endsWith("2"));

    List<String> result = repository.findBy(query).toList();

    assertEquals(1, result.size());
    assertEquals("item2", result.get(0));
  }

  @Test
  void shouldFindByRepositoryQueryWithPaginationOnly() {
    RepositoryCriteria<String, Predicate<String>> query = new RepositoryCriteria<>(1, 1);

    List<String> result = repository.findBy(query).toList();

    assertEquals(1, result.size());
    assertEquals("item2", result.get(0));
  }

  @Test
  void shouldCountWithFilter() {
    RepositoryCriteria<String, Predicate<String>> query =
        new RepositoryCriteria<>(item -> item.endsWith("1") || item.endsWith("3"));

    int count = repository.size(query);

    assertEquals(2, count);
  }

  @Test
  void shouldCountWithNoMatches() {
    RepositoryCriteria<String, Predicate<String>> query =
        new RepositoryCriteria<>(item -> item.startsWith("test"));

    int count = repository.size(query);

    assertEquals(0, count);
  }
}
