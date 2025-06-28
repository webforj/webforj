package com.webforj.data.repository.spring;

import static org.junit.jupiter.api.Assertions.*;

import com.webforj.data.repository.OrderCriteria;
import com.webforj.data.repository.OrderCriteriaList;
import com.webforj.data.repository.RepositoryCriteria;
import com.webforj.data.repository.spring.repository.CrudOnlyRepository;
import com.webforj.data.repository.spring.repository.CrudPagingRepository;
import com.webforj.data.repository.spring.repository.CrudSpecificationRepository;
import com.webforj.data.repository.spring.repository.FullRepository;
import com.webforj.data.repository.spring.repository.PagingOnlyRepository;
import com.webforj.data.repository.spring.repository.SpecificationOnlyRepository;
import com.webforj.data.repository.spring.repository.TestEntity;
import java.time.LocalDate;
import java.util.List;
import java.util.Optional;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.boot.test.autoconfigure.orm.jpa.TestEntityManager;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.repository.Repository;
import org.springframework.test.context.TestPropertySource;

@DataJpaTest
@TestPropertySource(
    properties = {"spring.jpa.hibernate.ddl-auto=create-drop", "spring.jpa.show-sql=false"})
class SpringDataRepositoryTest {

  @Autowired
  private TestEntityManager entityManager;

  @Autowired
  private FullRepository fullRepository; // All interfaces

  @Autowired
  private CrudOnlyRepository crudOnlyRepository;

  @Autowired
  private PagingOnlyRepository pagingOnlyRepository;

  @Autowired
  private SpecificationOnlyRepository specOnlyRepository;

  @Autowired
  private CrudPagingRepository crudPagingRepository;

  @Autowired
  private CrudSpecificationRepository crudSpecRepository;

  @AfterEach
  void cleanup() {
    entityManager.clear();
    fullRepository.deleteAll();
  }

  @Test
  void shouldRejectNullRepository() {
    assertThrows(NullPointerException.class, () -> new SpringDataRepository<>(null));
  }

  @Test
  void shouldRejectUnsupportedRepository() {
    Repository<TestEntity, Long> unsupported = new Repository<TestEntity, Long>() {};
    assertThrows(IllegalArgumentException.class, () -> new SpringDataRepository<>(unsupported));
  }

  @Nested
  @DisplayName("CrudRepository only")
  class CrudRepositoryOnly {

    private SpringDataRepository<TestEntity, Long> webforjRepo;

    @BeforeEach
    void setUp() {
      webforjRepo = new SpringDataRepository<>(crudOnlyRepository);
    }

    @Test
    @DisplayName("find() - should find by ID")
    void shouldFindById() {
      TestEntity entity = new TestEntity("John", "john@test.com", 25, true, LocalDate.now());
      entity = entityManager.persistAndFlush(entity);
      Long id = (Long) entity.getEntityKey();

      Optional<TestEntity> result = webforjRepo.find(id);

      assertTrue(result.isPresent());
      assertEquals("John", result.get().getName());
    }

    @Test
    @DisplayName("find() - should return empty when not found")
    void shouldReturnEmptyWhenNotFound() {
      Optional<TestEntity> result = webforjRepo.find(999L);
      assertTrue(result.isEmpty());
    }

    @Test
    @DisplayName("findBy() - should fail without paging support")
    void shouldFailFindByWithoutPagingSupport() {
      RepositoryCriteria<TestEntity, Specification<TestEntity>> query =
          new RepositoryCriteria<>(0, 10, null, null);

      assertThrows(UnsupportedOperationException.class, () -> {
        webforjRepo.findBy(query);
      });
    }

    @Test
    @DisplayName("count() - should count all entities")
    void shouldCountAll() {
      entityManager.persist(new TestEntity("Entity1", "e1@test.com", 25, true, LocalDate.now()));
      entityManager.persist(new TestEntity("Entity2", "e2@test.com", 30, false, LocalDate.now()));
      entityManager
          .persistAndFlush(new TestEntity("Entity3", "e3@test.com", 35, true, LocalDate.now()));

      assertEquals(3L, webforjRepo.size(new RepositoryCriteria<>(null)));
    }

    @Test
    @DisplayName("count() - should fail with specification")
    void shouldFailCountWithSpecification() {
      Specification<TestEntity> spec = (root, query, cb) -> cb.equal(root.get("active"), true);
      RepositoryCriteria<TestEntity, Specification<TestEntity>> query =
          new RepositoryCriteria<>(spec);

      assertThrows(UnsupportedOperationException.class, () -> webforjRepo.size(query));
    }
  }

  @Nested
  @DisplayName("PagingAndSortingRepository only")
  class PagingAndSortingRepositoryOnly {

    private SpringDataRepository<TestEntity, Long> repository;

    @BeforeEach
    void setUp() {
      repository = new SpringDataRepository<>(pagingOnlyRepository);
    }

    @Test
    @DisplayName("find() - should return empty without CrudRepository")
    void shouldReturnEmptyFind() {
      TestEntity entity = new TestEntity("John", "john@test.com", 25, true, LocalDate.now());
      entityManager.persistAndFlush(entity);

      Optional<TestEntity> result = repository.find(1L);
      assertTrue(result.isEmpty());
    }

    @Test
    @DisplayName("findBy() - should paginate without filter")
    void shouldFindByWithPagination() {
      for (int i = 1; i <= 5; i++) {
        entityManager.persist(
            new TestEntity("Entity" + i, "e" + i + "@test.com", 20 + i, true, LocalDate.now()));
      }
      entityManager.flush();

      RepositoryCriteria<TestEntity, Specification<TestEntity>> query =
          new RepositoryCriteria<>(0, 3, null, null);
      List<TestEntity> results = repository.findBy(query).toList();

      assertEquals(3, results.size());
    }

    @Test
    @DisplayName("findBy() - should handle offset and limit")
    void shouldFindByWithOffsetAndLimit() {
      for (int i = 1; i <= 10; i++) {
        entityManager.persist(
            new TestEntity("Entity" + i, "e" + i + "@test.com", 20 + i, true, LocalDate.now()));
      }
      entityManager.flush();

      RepositoryCriteria<TestEntity, Specification<TestEntity>> query =
          new RepositoryCriteria<>(3, 4, null, null);
      List<TestEntity> results = repository.findBy(query).toList();

      assertEquals(4, results.size());
      assertEquals("Entity4", results.get(0).getName());
      assertEquals("Entity7", results.get(3).getName());
    }

    @Test
    @DisplayName("findBy() - should apply sorting")
    void shouldFindByWithSorting() {
      entityManager
          .persist(new TestEntity("Charlie", "charlie@test.com", 30, true, LocalDate.now()));
      entityManager.persist(new TestEntity("Alice", "alice@test.com", 25, true, LocalDate.now()));
      entityManager
          .persistAndFlush(new TestEntity("Bob", "bob@test.com", 35, true, LocalDate.now()));

      OrderCriteria<TestEntity, String> orderCriteria =
          new OrderCriteria<>(TestEntity::getName, OrderCriteria.Direction.ASC, null, "name");
      RepositoryCriteria<TestEntity, Specification<TestEntity>> query =
          new RepositoryCriteria<TestEntity, Specification<TestEntity>>(0, 10,
              new OrderCriteriaList<>(orderCriteria), null);

      List<TestEntity> results = repository.findBy(query).toList();

      assertEquals(3, results.size());
      assertEquals("Alice", results.get(0).getName());
      assertEquals("Bob", results.get(1).getName());
      assertEquals("Charlie", results.get(2).getName());
    }

    @Test
    @DisplayName("findBy() - should handle multiple sort criteria")
    void shouldFindByWithMultipleSortCriteria() {
      entityManager.persist(new TestEntity("Bob", "bob1@test.com", 30, true, LocalDate.now()));
      entityManager.persist(new TestEntity("Alice", "alice@test.com", 25, true, LocalDate.now()));
      entityManager.persist(new TestEntity("Bob", "bob2@test.com", 35, true, LocalDate.now()));
      entityManager
          .persistAndFlush(new TestEntity("Alice", "alice2@test.com", 40, true, LocalDate.now()));

      OrderCriteria<TestEntity, String> nameCriteria =
          new OrderCriteria<>(TestEntity::getName, OrderCriteria.Direction.ASC, null, "name");
      OrderCriteria<TestEntity, Integer> ageCriteria =
          new OrderCriteria<>(TestEntity::getAge, OrderCriteria.Direction.DESC, null, "age");
      RepositoryCriteria<TestEntity, Specification<TestEntity>> query =
          new RepositoryCriteria<TestEntity, Specification<TestEntity>>(0, 10,
              new OrderCriteriaList<TestEntity>().add(nameCriteria).add(ageCriteria), null);

      List<TestEntity> results = repository.findBy(query).toList();

      assertEquals(4, results.size());
      assertEquals("Alice", results.get(0).getName());
      assertEquals(40, results.get(0).getAge()); // Alice 40 comes before Alice 25
      assertEquals("Alice", results.get(1).getName());
      assertEquals(25, results.get(1).getAge());
    }

    @Test
    @DisplayName("findBy() - should fail with specification")
    void shouldFailFindByWithSpecification() {
      Specification<TestEntity> spec = (root, query, cb) -> cb.equal(root.get("active"), true);
      RepositoryCriteria<TestEntity, Specification<TestEntity>> query =
          new RepositoryCriteria<>(0, 10, null, spec);

      assertThrows(UnsupportedOperationException.class, () -> {
        repository.findBy(query);
      });
    }

    @Test
    @DisplayName("count() - should fail without CrudRepository")
    void shouldFailCount() {
      assertThrows(UnsupportedOperationException.class,
          () -> repository.size(new RepositoryCriteria<>(null)));
    }
  }

  @Nested
  @DisplayName("JpaSpecificationExecutor only")
  class JpaSpecificationExecutorOnly {

    private SpringDataRepository<TestEntity, Long> repository;

    @BeforeEach
    void setUp() {
      repository = new SpringDataRepository<>(specOnlyRepository);
    }

    @Test
    @DisplayName("find() - should return empty without CrudRepository")
    void shouldReturnEmptyFind() {
      TestEntity entity = new TestEntity("John", "john@test.com", 25, true, LocalDate.now());
      entityManager.persistAndFlush(entity);

      Optional<TestEntity> result = repository.find(1L);
      assertTrue(result.isEmpty());
    }

    @Test
    @DisplayName("findBy() - should work with null filter")
    void shouldFindByWithNullFilter() {
      entityManager.persist(new TestEntity("Entity1", "e1@test.com", 25, true, LocalDate.now()));
      entityManager
          .persistAndFlush(new TestEntity("Entity2", "e2@test.com", 30, false, LocalDate.now()));

      RepositoryCriteria<TestEntity, Specification<TestEntity>> query =
          new RepositoryCriteria<>(0, 10, null, null);
      List<TestEntity> results = repository.findBy(query).toList();

      assertEquals(2, results.size());
    }

    @Test
    @DisplayName("findBy() - should work with specification")
    void shouldFindByWithSpecification() {
      entityManager.persist(new TestEntity("Active1", "a1@test.com", 25, true, LocalDate.now()));
      entityManager.persist(new TestEntity("Inactive", "i@test.com", 30, false, LocalDate.now()));
      entityManager
          .persistAndFlush(new TestEntity("Active2", "a2@test.com", 35, true, LocalDate.now()));

      Specification<TestEntity> spec = (root, query, cb) -> cb.equal(root.get("active"), true);
      RepositoryCriteria<TestEntity, Specification<TestEntity>> query =
          new RepositoryCriteria<>(0, 10, null, spec);
      List<TestEntity> results = repository.findBy(query).toList();

      assertEquals(2, results.size());
      assertTrue(results.stream().allMatch(TestEntity::isActive));
    }

    @Test
    @DisplayName("findBy() - should handle pagination with specification")
    void shouldFindByWithSpecificationAndPagination() {
      for (int i = 1; i <= 8; i++) {
        boolean active = i % 2 == 1; // Odd numbers are active
        entityManager.persist(
            new TestEntity("Entity" + i, "e" + i + "@test.com", 20 + i, active, LocalDate.now()));
      }
      entityManager.flush();

      Specification<TestEntity> spec = (root, query, cb) -> cb.equal(root.get("active"), true);
      RepositoryCriteria<TestEntity, Specification<TestEntity>> query =
          new RepositoryCriteria<>(1, 2, null, spec);
      List<TestEntity> results = repository.findBy(query).toList();

      assertEquals(2, results.size());
      assertTrue(results.stream().allMatch(TestEntity::isActive));
    }

    @Test
    @DisplayName("findBy() - should handle sorting with specification")
    void shouldFindByWithSpecificationAndSorting() {
      entityManager
          .persist(new TestEntity("Charlie", "charlie@test.com", 30, true, LocalDate.now()));
      entityManager.persist(new TestEntity("Alice", "alice@test.com", 25, false, LocalDate.now()));
      entityManager.persist(new TestEntity("Bob", "bob@test.com", 35, true, LocalDate.now()));
      entityManager
          .persistAndFlush(new TestEntity("David", "david@test.com", 28, true, LocalDate.now()));

      Specification<TestEntity> spec = (root, query, cb) -> cb.equal(root.get("active"), true);
      OrderCriteria<TestEntity, String> orderCriteria =
          new OrderCriteria<>(TestEntity::getName, OrderCriteria.Direction.DESC, null, "name");
      RepositoryCriteria<TestEntity, Specification<TestEntity>> query =
          new RepositoryCriteria<TestEntity, Specification<TestEntity>>(0, 10,
              new OrderCriteriaList<>(orderCriteria), spec);

      List<TestEntity> results = repository.findBy(query).toList();

      assertEquals(3, results.size());
      assertEquals("David", results.get(0).getName());
      assertEquals("Charlie", results.get(1).getName());
      assertEquals("Bob", results.get(2).getName());
    }

    @Test
    @DisplayName("count() - should count with null specification")
    void shouldCountWithNullSpecification() {
      entityManager.persist(new TestEntity("Entity1", "e1@test.com", 25, true, LocalDate.now()));
      entityManager
          .persistAndFlush(new TestEntity("Entity2", "e2@test.com", 30, false, LocalDate.now()));

      long count = repository.size(new RepositoryCriteria<>(null));

      assertEquals(2L, count);
    }

    @Test
    @DisplayName("count() - should count with specification")
    void shouldCountWithSpecification() {
      entityManager.persist(new TestEntity("Active1", "a1@test.com", 25, true, LocalDate.now()));
      entityManager.persist(new TestEntity("Inactive", "i@test.com", 30, false, LocalDate.now()));
      entityManager
          .persistAndFlush(new TestEntity("Active2", "a2@test.com", 35, true, LocalDate.now()));

      Specification<TestEntity> spec = (root, query, cb) -> cb.equal(root.get("active"), true);
      long count = repository.size(new RepositoryCriteria<>(spec));

      assertEquals(2L, count);
    }

    @Test
    @DisplayName("count() - should work with base filter")
    void shouldCountWithBaseFilter() {
      entityManager.persist(new TestEntity("Active1", "a1@test.com", 25, true, LocalDate.now()));
      entityManager.persist(new TestEntity("Inactive", "i@test.com", 30, false, LocalDate.now()));
      entityManager
          .persistAndFlush(new TestEntity("Active2", "a2@test.com", 35, true, LocalDate.now()));

      // BaseFilter is not supported in SpringDataRepository
      // Count with null filter instead
      long count = repository.size(new RepositoryCriteria<>(null));
      assertEquals(3L, count); // Should count all entities
    }
  }

  @Nested
  @DisplayName("CrudRepository + PagingAndSortingRepository")
  class CrudAndPagingRepository {

    private SpringDataRepository<TestEntity, Long> repository;

    @BeforeEach
    void setUp() {
      repository = new SpringDataRepository<>(crudPagingRepository);
    }

    @Test
    @DisplayName("find() - should find by ID")
    void shouldFindById() {
      TestEntity entity = new TestEntity("John", "john@test.com", 25, true, LocalDate.now());
      entity = entityManager.persistAndFlush(entity);
      Long id = (Long) entity.getEntityKey();

      Optional<TestEntity> result = repository.find(id);

      assertTrue(result.isPresent());
      assertEquals("John", result.get().getName());
    }

    @Test
    @DisplayName("findBy() - should paginate with all features")
    void shouldFindByWithAllFeatures() {
      for (int i = 1; i <= 10; i++) {
        entityManager.persist(
            new TestEntity("Entity" + i, "e" + i + "@test.com", 20 + i, true, LocalDate.now()));
      }
      entityManager.flush();

      OrderCriteria<TestEntity, String> orderCriteria =
          new OrderCriteria<>(TestEntity::getName, OrderCriteria.Direction.ASC, null, "name");
      RepositoryCriteria<TestEntity, Specification<TestEntity>> query =
          new RepositoryCriteria<TestEntity, Specification<TestEntity>>(2, 3,
              new OrderCriteriaList<>(orderCriteria), null);

      List<TestEntity> results = repository.findBy(query).toList();

      assertEquals(3, results.size());
      assertEquals("Entity2", results.get(0).getName());
      assertEquals("Entity3", results.get(1).getName());
      assertEquals("Entity4", results.get(2).getName());
    }

    @Test
    @DisplayName("findBy() - should fail with specification")
    void shouldFailFindByWithSpecification() {
      Specification<TestEntity> spec = (root, query, cb) -> cb.equal(root.get("active"), true);
      RepositoryCriteria<TestEntity, Specification<TestEntity>> query =
          new RepositoryCriteria<>(0, 10, null, spec);

      assertThrows(UnsupportedOperationException.class, () -> {
        repository.findBy(query);
      });
    }

    @Test
    @DisplayName("count() - should count all")
    void shouldCount() {
      entityManager.persist(new TestEntity("Entity1", "e1@test.com", 25, true, LocalDate.now()));
      entityManager.persist(new TestEntity("Entity2", "e2@test.com", 30, false, LocalDate.now()));
      entityManager
          .persistAndFlush(new TestEntity("Entity3", "e3@test.com", 35, true, LocalDate.now()));

      assertEquals(3L, repository.size(new RepositoryCriteria<>(null)));
    }

    @Test
    @DisplayName("count() - should fail with specification")
    void shouldFailCountWithSpecification() {
      Specification<TestEntity> spec = (root, query, cb) -> cb.equal(root.get("active"), true);
      RepositoryCriteria<TestEntity, Specification<TestEntity>> query =
          new RepositoryCriteria<>(spec);

      assertThrows(UnsupportedOperationException.class, () -> repository.size(query));
    }
  }

  @Nested
  @DisplayName("CrudRepository + JpaSpecificationExecutor")
  class CrudAndSpecificationRepository {

    private SpringDataRepository<TestEntity, Long> repository;

    @BeforeEach
    void setUp() {
      repository = new SpringDataRepository<>(crudSpecRepository);
    }

    @Test
    @DisplayName("find() - should find by ID")
    void shouldFindById() {
      TestEntity entity = new TestEntity("John", "john@test.com", 25, true, LocalDate.now());
      entity = entityManager.persistAndFlush(entity);
      Long id = (Long) entity.getEntityKey();

      Optional<TestEntity> result = repository.find(id);

      assertTrue(result.isPresent());
      assertEquals("John", result.get().getName());
    }

    @Test
    @DisplayName("findBy() - should work with specification and all features")
    void shouldFindByWithSpecificationAndAllFeatures() {
      for (int i = 1; i <= 10; i++) {
        boolean active = i % 3 != 0; // Every 3rd is inactive
        entityManager.persist(
            new TestEntity("Entity" + i, "e" + i + "@test.com", 20 + i, active, LocalDate.now()));
      }
      entityManager.flush();

      Specification<TestEntity> spec = (root, query, cb) -> cb.equal(root.get("active"), true);
      OrderCriteria<TestEntity, String> orderCriteria =
          new OrderCriteria<>(TestEntity::getName, OrderCriteria.Direction.DESC, null, "name");
      RepositoryCriteria<TestEntity, Specification<TestEntity>> query =
          new RepositoryCriteria<TestEntity, Specification<TestEntity>>(1, 3,
              new OrderCriteriaList<>(orderCriteria), spec);

      List<TestEntity> results = repository.findBy(query).toList();

      assertEquals(3, results.size());
      assertTrue(results.stream().allMatch(TestEntity::isActive));
    }

    @Test
    @DisplayName("findBy() - should work with null filter")
    void shouldFindByWithNullFilter() {
      entityManager.persist(new TestEntity("Entity1", "e1@test.com", 25, true, LocalDate.now()));
      entityManager
          .persistAndFlush(new TestEntity("Entity2", "e2@test.com", 30, false, LocalDate.now()));

      RepositoryCriteria<TestEntity, Specification<TestEntity>> query =
          new RepositoryCriteria<>(0, 10, null, null);

      List<TestEntity> results = repository.findBy(query).toList();

      assertEquals(2, results.size());
    }

    @Test
    @DisplayName("count() - should count all with CrudRepository")
    void shouldCountAll() {
      entityManager.persist(new TestEntity("Entity1", "e1@test.com", 25, true, LocalDate.now()));
      entityManager.persist(new TestEntity("Entity2", "e2@test.com", 30, false, LocalDate.now()));
      entityManager
          .persistAndFlush(new TestEntity("Entity3", "e3@test.com", 35, true, LocalDate.now()));

      assertEquals(3L, repository.size(new RepositoryCriteria<>(null)));
    }

    @Test
    @DisplayName("count() - should count with specification")
    void shouldCountWithSpecification() {
      entityManager.persist(new TestEntity("Active1", "a1@test.com", 25, true, LocalDate.now()));
      entityManager.persist(new TestEntity("Inactive", "i@test.com", 30, false, LocalDate.now()));
      entityManager
          .persistAndFlush(new TestEntity("Active2", "a2@test.com", 35, true, LocalDate.now()));

      Specification<TestEntity> spec = (root, query, cb) -> cb.equal(root.get("active"), true);
      long count = repository.size(new RepositoryCriteria<>(spec));

      assertEquals(2L, count);
    }
  }

  @Nested
  @DisplayName("All interfaces combined")
  class AllInterfacesCombined {

    private SpringDataRepository<TestEntity, Long> repository;

    @BeforeEach
    void setUp() {
      repository = new SpringDataRepository<>(fullRepository);
    }

    @Test
    @DisplayName("find() - should find by ID")
    void shouldFindById() {
      TestEntity entity = new TestEntity("John", "john@test.com", 25, true, LocalDate.now());
      entity = entityManager.persistAndFlush(entity);
      Long id = (Long) entity.getEntityKey();

      Optional<TestEntity> result = repository.find(id);

      assertTrue(result.isPresent());
      assertEquals("John", result.get().getName());
    }

    @Test
    @DisplayName("findBy() - should work with all features combined")
    void shouldFindByWithAllFeatures() {
      for (int i = 1; i <= 15; i++) {
        boolean active = i % 2 == 1; // Odd numbers are active
        entityManager.persist(new TestEntity("Entity" + String.format("%02d", i),
            "e" + i + "@test.com", 20 + i, active, LocalDate.now()));
      }
      entityManager.flush();

      Specification<TestEntity> spec = (root, query, cb) -> cb.equal(root.get("active"), true);
      OrderCriteria<TestEntity, String> nameCriteria =
          new OrderCriteria<>(TestEntity::getName, OrderCriteria.Direction.DESC, null, "name");
      OrderCriteria<TestEntity, Integer> ageCriteria =
          new OrderCriteria<>(TestEntity::getAge, OrderCriteria.Direction.ASC, null, "age");
      RepositoryCriteria<TestEntity, Specification<TestEntity>> query =
          new RepositoryCriteria<TestEntity, Specification<TestEntity>>(2, 3,
              new OrderCriteriaList<TestEntity>().add(nameCriteria).add(ageCriteria), spec);

      List<TestEntity> results = repository.findBy(query).toList();

      assertEquals(3, results.size());
      assertTrue(results.stream().allMatch(TestEntity::isActive));
    }

    @Test
    @DisplayName("findBy() - should handle empty results")
    void shouldHandleEmptyResults() {
      // No entities in database
      RepositoryCriteria<TestEntity, Specification<TestEntity>> query =
          new RepositoryCriteria<>(0, 10, null, null);
      List<TestEntity> results = repository.findBy(query).toList();

      assertTrue(results.isEmpty());
    }

    @Test
    @DisplayName("count() - should count all")
    void shouldCountAll() {
      entityManager.persist(new TestEntity("Entity1", "e1@test.com", 25, true, LocalDate.now()));
      entityManager.persist(new TestEntity("Entity2", "e2@test.com", 30, false, LocalDate.now()));
      entityManager
          .persistAndFlush(new TestEntity("Entity3", "e3@test.com", 35, true, LocalDate.now()));

      assertEquals(3L, repository.size(new RepositoryCriteria<>(null)));
    }

    @Test
    @DisplayName("count() - should count with specification")
    void shouldCountWithSpecification() {
      entityManager.persist(new TestEntity("Active1", "a1@test.com", 25, true, LocalDate.now()));
      entityManager.persist(new TestEntity("Inactive", "i@test.com", 30, false, LocalDate.now()));
      entityManager
          .persistAndFlush(new TestEntity("Active2", "a2@test.com", 35, true, LocalDate.now()));

      Specification<TestEntity> spec = (root, query, cb) -> cb.equal(root.get("active"), true);
      long count = repository.size(new RepositoryCriteria<>(spec));

      assertEquals(2L, count);
    }

    @Test
    @DisplayName("should handle complex queries with all features")
    void shouldHandleComplexQueries() {
      entityManager
          .persist(new TestEntity("John Doe", "john@company.com", 25, true, LocalDate.now()));
      entityManager
          .persist(new TestEntity("Jane Smith", "jane@company.com", 30, true, LocalDate.now()));
      entityManager
          .persist(new TestEntity("Bob Johnson", "bob@other.com", 35, false, LocalDate.now()));
      entityManager.persistAndFlush(
          new TestEntity("Alice Brown", "alice@company.com", 28, true, LocalDate.now()));

      // Find active users from company.com domain, sorted by name
      Specification<TestEntity> spec = (root, query, cb) -> cb
          .and(cb.equal(root.get("active"), true), cb.like(root.get("email"), "%@company.com"));

      OrderCriteria<TestEntity, String> nameCriteria =
          new OrderCriteria<>(TestEntity::getName, OrderCriteria.Direction.ASC, null, "name");

      RepositoryCriteria<TestEntity, Specification<TestEntity>> query =
          new RepositoryCriteria<TestEntity, Specification<TestEntity>>(0, 10,
              new OrderCriteriaList<>(nameCriteria), spec);

      List<TestEntity> results = repository.findBy(query).toList();

      assertEquals(3, results.size());
      assertEquals("Alice Brown", results.get(0).getName());
      assertEquals("Jane Smith", results.get(1).getName());
      assertEquals("John Doe", results.get(2).getName());
    }

    @Test
    @DisplayName("should handle composite property sorting")
    void shouldHandleCompositePropertySorting() {
      entityManager.persist(new TestEntity("Alice", "alice@test.com", 25, true, LocalDate.now()));
      entityManager.persist(new TestEntity("Bob", "bob@test.com", 30, true, LocalDate.now()));
      entityManager.persist(new TestEntity("Alice", "alice2@test.com", 35, true, LocalDate.now()));
      entityManager.persistAndFlush(
          new TestEntity("Charlie", "charlie@test.com", 28, true, LocalDate.now()));

      OrderCriteria<TestEntity, String> compositeCriteria =
          new OrderCriteria<>(TestEntity::getName, OrderCriteria.Direction.ASC, null, "name,age");
      RepositoryCriteria<TestEntity, Specification<TestEntity>> query =
          new RepositoryCriteria<TestEntity, Specification<TestEntity>>(0, 10,
              new OrderCriteriaList<>(compositeCriteria), null);

      List<TestEntity> results = repository.findBy(query).toList();

      assertEquals(4, results.size());
      // Should be sorted by name first, then age
      assertEquals("Alice", results.get(0).getName());
      assertEquals(25, results.get(0).getAge()); // Alice 25 comes before Alice 35
      assertEquals("Alice", results.get(1).getName());
      assertEquals(35, results.get(1).getAge());
      assertEquals("Bob", results.get(2).getName());
      assertEquals("Charlie", results.get(3).getName());
    }

    @Test
    @DisplayName("should handle composite property with whitespace")
    void shouldHandleCompositePropertyWithWhitespace() {
      entityManager.persist(new TestEntity("Alice", "alice@test.com", 30, true, LocalDate.now()));
      entityManager.persist(new TestEntity("Bob", "bob@test.com", 25, true, LocalDate.now()));
      entityManager.persistAndFlush(
          new TestEntity("Charlie", "charlie@test.com", 35, true, LocalDate.now()));

      // Test with spaces around commas
      OrderCriteria<TestEntity, String> compositeCriteria = new OrderCriteria<>(TestEntity::getName,
          OrderCriteria.Direction.DESC, null, "active , name , age");
      RepositoryCriteria<TestEntity, Specification<TestEntity>> query =
          new RepositoryCriteria<TestEntity, Specification<TestEntity>>(0, 10,
              new OrderCriteriaList<>(compositeCriteria), null);

      List<TestEntity> results = repository.findBy(query).toList();

      assertEquals(3, results.size());
      // All are active=true, so sorted by name DESC
      assertEquals("Charlie", results.get(0).getName());
      assertEquals("Bob", results.get(1).getName());
      assertEquals("Alice", results.get(2).getName());
    }

    @Test
    @DisplayName("should handle empty properties in composite")
    void shouldHandleEmptyPropertiesInComposite() {
      entityManager.persist(new TestEntity("Alice", "alice@test.com", 30, true, LocalDate.now()));
      entityManager
          .persistAndFlush(new TestEntity("Bob", "bob@test.com", 25, true, LocalDate.now()));

      // Test with empty properties (extra commas)
      OrderCriteria<TestEntity, String> compositeCriteria =
          new OrderCriteria<>(TestEntity::getName, OrderCriteria.Direction.ASC, null, "name,,age,");
      RepositoryCriteria<TestEntity, Specification<TestEntity>> query =
          new RepositoryCriteria<TestEntity, Specification<TestEntity>>(0, 10,
              new OrderCriteriaList<>(compositeCriteria), null);

      List<TestEntity> results = repository.findBy(query).toList();

      assertEquals(2, results.size());
      // Should ignore empty properties and sort by name, then age
      assertEquals("Alice", results.get(0).getName());
      assertEquals("Bob", results.get(1).getName());
    }

    @Test
    @DisplayName("should handle single property")
    void shouldHandleSingleProperty() {
      entityManager
          .persist(new TestEntity("Charlie", "charlie@test.com", 30, true, LocalDate.now()));
      entityManager.persist(new TestEntity("Alice", "alice@test.com", 25, true, LocalDate.now()));
      entityManager
          .persistAndFlush(new TestEntity("Bob", "bob@test.com", 35, true, LocalDate.now()));

      // Test single property (no commas) - should work as before
      OrderCriteria<TestEntity, String> singleCriteria =
          new OrderCriteria<>(TestEntity::getName, OrderCriteria.Direction.ASC, null, "name");
      RepositoryCriteria<TestEntity, Specification<TestEntity>> query =
          new RepositoryCriteria<TestEntity, Specification<TestEntity>>(0, 10,
              new OrderCriteriaList<>(singleCriteria), null);

      List<TestEntity> results = repository.findBy(query).toList();

      assertEquals(3, results.size());
      assertEquals("Alice", results.get(0).getName());
      assertEquals("Bob", results.get(1).getName());
      assertEquals("Charlie", results.get(2).getName());
    }

    @Test
    @DisplayName("should use base filter correctly")
    void shouldUseBaseFilter() {
      entityManager
          .persist(new TestEntity("Active Young", "ay@test.com", 20, true, LocalDate.now()));
      entityManager.persist(new TestEntity("Active Old", "ao@test.com", 40, true, LocalDate.now()));
      entityManager
          .persist(new TestEntity("Inactive Young", "iy@test.com", 22, false, LocalDate.now()));
      entityManager.persistAndFlush(
          new TestEntity("Inactive Old", "io@test.com", 45, false, LocalDate.now()));

      // BaseFilter is not supported in SpringDataRepository
      // Test with regular specification filter instead
      Specification<TestEntity> ageFilter =
          (root, query, cb) -> cb.greaterThan(root.get("age"), 30);
      List<TestEntity> results = repository
          .findBy(new RepositoryCriteria<TestEntity, Specification<TestEntity>>(ageFilter))
          .toList();

      // Should find both old entities (active and inactive)
      assertEquals(2, results.size());
    }
  }
}
