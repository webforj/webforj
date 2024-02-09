package org.dwcj.data.repository;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertIterableEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;
import org.dwcj.data.repository.event.RepositoryCommitEvent;
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
  void shouldClearRepository() {
    repository.clear();
    int count = repository.size();
    assertEquals(0, count);
  }

  @Test
  void shouldFindByIndex() {
    Optional<String> result = repository.findByIndex(1);
    assertTrue(result.isPresent());
    assertEquals("item2", result.get());
  }

  @Test
  void shouldNotFindByInvalidIndex() {
    Optional<String> result = repository.findByIndex(10);
    assertTrue(result.isEmpty());
  }

  @Test
  void shouldFindByCriteria() {
    RetrievalBuilder<String> criteria = new RetrievalBuilder<String>(1, // offset
        2, // limit
        Comparator.naturalOrder(), // orderBy
        item -> item.startsWith("item") // filter
    );

    List<String> result = repository.findBy(criteria).toList();

    assertIterableEquals(Arrays.asList("item2", "item3"), result);
  }

  @Test
  void shouldFindAllByDefaultCriteria() {
    repository.setOffset(1);
    repository.setLimit(2);
    repository.setOrderBy(Comparator.naturalOrder());
    repository.setFilter(item -> item.startsWith("item"));
    List<String> result = repository.findAll().toList();

    assertIterableEquals(Arrays.asList("item2", "item3"), result);
  }

  @Test
  void shouldGetIndex() {
    int index = repository.getIndex("item2");
    assertEquals(1, index);
  }

  @Test
  void shouldNotGetIndexForNonExistentItem() {
    int index = repository.getIndex("item4");
    assertEquals(-1, index);
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
}
