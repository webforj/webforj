package com.webforj.data.repository;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.Arrays;
import java.util.List;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class QueryableRepositoryTest {

  private CollectionRepository<TestEntity> repository;
  private List<TestEntity> testData;

  @BeforeEach
  void setUp() {
    testData = Arrays.asList(new TestEntity("1", "Alice", 25), new TestEntity("2", "Bob", 30),
        new TestEntity("3", "Charlie", 35), new TestEntity("4", "David", 40),
        new TestEntity("5", "Eve", 45));
    repository = new CollectionRepository<>(testData);
  }

  @Test
  void shouldFindAllWithFilter() {
    Predicate<TestEntity> filter = e -> e.getAge() > 30;
    RepositoryCriteria<TestEntity, Predicate<TestEntity>> query =
        new RepositoryCriteria<>(0, Integer.MAX_VALUE, null, filter);

    List<TestEntity> result = repository.findBy(query).collect(Collectors.toList());

    assertEquals(3, result.size());
    assertEquals("Charlie", result.get(0).getName());
    assertEquals("David", result.get(1).getName());
    assertEquals("Eve", result.get(2).getName());
  }

  @Test
  void shouldFindAllWithPagination() {
    RepositoryCriteria<TestEntity, Predicate<TestEntity>> query =
        new RepositoryCriteria<>(1, 2, null, null);

    List<TestEntity> result = repository.findBy(query).collect(Collectors.toList());

    assertEquals(2, result.size());
    assertEquals("Bob", result.get(0).getName());
    assertEquals("Charlie", result.get(1).getName());
  }

  @Test
  void shouldFindAllWithSorting() {
    var orderCriteriaList = new OrderCriteriaList<TestEntity>();
    orderCriteriaList.add(new OrderCriteria<>(TestEntity::getAge, OrderCriteria.Direction.DESC));

    RepositoryCriteria<TestEntity, Predicate<TestEntity>> query =
        new RepositoryCriteria<>(0, Integer.MAX_VALUE, orderCriteriaList, null);

    List<TestEntity> result = repository.findBy(query).collect(Collectors.toList());

    assertEquals(5, result.size());
    assertEquals("Eve", result.get(0).getName());
    assertEquals("David", result.get(1).getName());
    assertEquals("Charlie", result.get(2).getName());
  }

  @Test
  void shouldCountWithFilter() {
    Predicate<TestEntity> filter = e -> e.getAge() >= 35;
    RepositoryCriteria<TestEntity, Predicate<TestEntity>> query =
        new RepositoryCriteria<>(0, Integer.MAX_VALUE, null, filter);

    int count = repository.size(query);

    assertEquals(3, count);
  }

  @Test
  void shouldHandleComplexQuery() {
    // Filter for age > 25 and sort by name ascending
    Predicate<TestEntity> filter = e -> e.getAge() > 25;
    var orderCriteriaList = new OrderCriteriaList<TestEntity>();
    orderCriteriaList.add(new OrderCriteria<>(TestEntity::getName, OrderCriteria.Direction.ASC));

    RepositoryCriteria<TestEntity, Predicate<TestEntity>> query =
        new RepositoryCriteria<>(1, 2, orderCriteriaList, filter);

    List<TestEntity> result = repository.findBy(query).collect(Collectors.toList());

    assertEquals(2, result.size());
    assertEquals("Charlie", result.get(0).getName());
    assertEquals("David", result.get(1).getName());
  }

  @Test
  void shouldFindAllWithFilterOnlyConstructor() {
    Predicate<TestEntity> filter = e -> e.getAge() > 35;
    RepositoryCriteria<TestEntity, Predicate<TestEntity>> query = new RepositoryCriteria<>(filter);

    List<TestEntity> result = repository.findBy(query).collect(Collectors.toList());

    assertEquals(2, result.size());
    assertEquals("David", result.get(0).getName());
    assertEquals("Eve", result.get(1).getName());
  }

  @Test
  void shouldFindAllWithPaginationOnlyConstructor() {
    RepositoryCriteria<TestEntity, Predicate<TestEntity>> query = new RepositoryCriteria<>(2, 2);

    List<TestEntity> result = repository.findBy(query).collect(Collectors.toList());

    assertEquals(2, result.size());
    assertEquals("Charlie", result.get(0).getName());
    assertEquals("David", result.get(1).getName());
  }

  @Test
  void shouldFindAllWithFilterAndPaginationConstructor() {
    Predicate<TestEntity> filter = e -> e.getAge() >= 30;
    RepositoryCriteria<TestEntity, Predicate<TestEntity>> query =
        new RepositoryCriteria<>(0, 2, filter);

    List<TestEntity> result = repository.findBy(query).collect(Collectors.toList());

    assertEquals(2, result.size());
    assertEquals("Bob", result.get(0).getName());
    assertEquals("Charlie", result.get(1).getName());
  }

  static class TestEntity {
    private final String id;
    private final String name;
    private final int age;

    TestEntity(String id, String name, int age) {
      this.id = id;
      this.name = name;
      this.age = age;
    }

    public String getId() {
      return id;
    }

    public String getName() {
      return name;
    }

    public int getAge() {
      return age;
    }
  }
}
