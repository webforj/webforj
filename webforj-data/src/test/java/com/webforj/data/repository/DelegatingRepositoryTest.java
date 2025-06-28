package com.webforj.data.repository;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.data.HasEntityKey;
import com.webforj.data.repository.event.RepositoryCommitEvent;
import com.webforj.dispatcher.EventListener;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class DelegatingRepositoryTest {

  private CollectionRepository<TestEntity> backingRepo;
  private DelegatingRepository<TestEntity, Predicate<TestEntity>> delegatingRepo;
  private List<TestEntity> testData;

  static class TestEntity implements HasEntityKey {
    private final Long id;
    private final String name;
    private final int value;

    TestEntity(Long id, String name, int value) {
      this.id = id;
      this.name = name;
      this.value = value;
    }

    Long getId() {
      return id;
    }

    String getName() {
      return name;
    }

    int getValue() {
      return value;
    }

    @Override
    public Object getEntityKey() {
      return id;
    }
  }

  @BeforeEach
  void setUp() {
    testData = Arrays.asList(new TestEntity(1L, "Alpha", 10), new TestEntity(2L, "Beta", 20),
        new TestEntity(3L, "Gamma", 30), new TestEntity(4L, "Delta", 40),
        new TestEntity(5L, "Epsilon", 50));

    backingRepo = new CollectionRepository<TestEntity>(testData);

    // @formatter:off
    delegatingRepo = new DelegatingRepository<>(
        query -> backingRepo.findBy(query),
        query -> backingRepo.size(query),
        key -> backingRepo.find(key)
    );
    // @formatter:on
  }

  @Test
  void shouldRequireNonNullCallbacksInConstructor() {
    assertThrows(NullPointerException.class,
        () -> new DelegatingRepository<>(null, query -> 0, key -> Optional.empty()));

    assertThrows(NullPointerException.class,
        () -> new DelegatingRepository<>(query -> Stream.empty(), null, key -> Optional.empty()));

    assertThrows(NullPointerException.class,
        () -> new DelegatingRepository<>(query -> Stream.empty(), query -> 0, null));
  }

  @Test
  void shouldFindAllEntities() {
    List<TestEntity> result = delegatingRepo.findAll().toList();
    assertEquals(5, result.size());
    assertEquals("Alpha", result.get(0).getName());
  }

  @Test
  void shouldFindAllWithPagination() {
    delegatingRepo.setOffset(1);
    delegatingRepo.setLimit(3);

    List<TestEntity> result = delegatingRepo.findAll().toList();
    assertEquals(3, result.size());
    assertEquals("Beta", result.get(0).getName());
  }

  @Test
  void shouldFindAllWithSorting() {
    OrderCriteria<TestEntity, Integer> orderByValue =
        new OrderCriteria<>(TestEntity::getValue, OrderCriteria.Direction.DESC);
    delegatingRepo.getOrderCriteriaList().add(orderByValue);

    List<TestEntity> result = delegatingRepo.findAll().toList();
    assertEquals("Epsilon", result.get(0).getName());
    assertEquals("Delta", result.get(1).getName());
  }

  @Test
  void shouldFindAllWithBaseFilter() {
    delegatingRepo.setBaseFilter(entity -> entity.getValue() > 25);

    List<TestEntity> result = delegatingRepo.findAll().toList();
    assertEquals(3, result.size());
    assertTrue(result.stream().allMatch(e -> e.getValue() > 25));
  }

  @Test
  void shouldFindByQuery() {
    RepositoryCriteria<TestEntity, Predicate<TestEntity>> query =
        new RepositoryCriteria<>(1, 3, null, entity -> entity.getValue() >= 30);

    List<TestEntity> result = delegatingRepo.findBy(query).collect(Collectors.toList());
    assertEquals(2, result.size());
    assertEquals("Delta", result.get(0).getName());
    assertEquals("Epsilon", result.get(1).getName());
  }

  @Test
  void shouldCountAllEntities() {
    assertEquals(5, delegatingRepo.size());
  }

  @Test
  void shouldCountWithBaseFilter() {
    delegatingRepo.setBaseFilter(entity -> entity.getValue() < 30);
    assertEquals(2, delegatingRepo.size());
  }

  @Test
  void shouldCountWithQuery() {
    RepositoryCriteria<TestEntity, Predicate<TestEntity>> query =
        new RepositoryCriteria<>(entity -> entity.getName().startsWith("E"));

    assertEquals(1, delegatingRepo.size(query));
  }

  @Test
  void shouldFindEntityByKey() {
    Optional<TestEntity> result = delegatingRepo.find(3L);
    assertTrue(result.isPresent());
    assertEquals("Gamma", result.get().getName());
  }

  @Test
  void shouldReturnEmptyWhenEntityNotFound() {
    Optional<TestEntity> result = delegatingRepo.find(999L);
    assertFalse(result.isPresent());
  }

  @Test
  void shouldFireEventOnCommit() {
    AtomicInteger eventCount = new AtomicInteger(0);
    EventListener<RepositoryCommitEvent<TestEntity>> listener =
        event -> eventCount.incrementAndGet();
    delegatingRepo.addCommitListener(listener);

    delegatingRepo.commit();

    assertEquals(1, eventCount.get());
  }

  @Test
  void shouldFireEventOnSingleEntityCommit() {
    AtomicInteger eventCount = new AtomicInteger(0);
    EventListener<RepositoryCommitEvent<TestEntity>> listener = event -> {
      assertTrue(event.isSingleCommit());
      assertEquals(1, event.getCommits().size());
      eventCount.incrementAndGet();
    };
    delegatingRepo.addCommitListener(listener);

    delegatingRepo.commit(testData.get(0));

    assertEquals(1, eventCount.get());
  }

  @Test
  void shouldInvokeCallbacksCorrectly() {
    AtomicInteger fetchCount = new AtomicInteger(0);
    AtomicInteger countCount = new AtomicInteger(0);
    AtomicInteger findCount = new AtomicInteger(0);

    DelegatingRepository<TestEntity, Predicate<TestEntity>> spyRepo =
        new DelegatingRepository<>(query -> {
          fetchCount.incrementAndGet();
          return backingRepo.findBy(query);
        }, query -> {
          countCount.incrementAndGet();
          return backingRepo.size(query);
        }, key -> {
          findCount.incrementAndGet();
          return backingRepo.find(key);
        });

    // Test fetch callback
    spyRepo.findAll().toList();
    assertEquals(1, fetchCount.get());

    // Test count callback
    spyRepo.size();
    assertEquals(1, countCount.get());

    // Test find callback
    spyRepo.find(1L);
    assertEquals(1, findCount.get());
  }

  @Test
  void shouldRespectAllParametersInFindAll() {
    delegatingRepo.setOffset(1);
    delegatingRepo.setLimit(3);
    delegatingRepo.setBaseFilter(entity -> entity.getValue() >= 20);
    delegatingRepo.getOrderCriteriaList()
        .add(new OrderCriteria<>(TestEntity::getName, OrderCriteria.Direction.DESC));

    List<TestEntity> result = delegatingRepo.findAll().collect(Collectors.toList());

    // With filter (>=20): Beta(20), Gamma(30), Delta(40), Epsilon(50)
    // Sorted DESC by name: Gamma, Epsilon, Delta, Beta
    // Skip 1 (start=1): Epsilon, Delta, Beta
    // Limit 3: Epsilon, Delta, Beta
    assertEquals(3, result.size());
    assertEquals("Epsilon", result.get(0).getName());
    assertEquals("Delta", result.get(1).getName());
    assertEquals("Beta", result.get(2).getName());
  }
}
