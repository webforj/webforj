package com.webforj.data.repository;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.webforj.data.HasEntityKey;
import com.webforj.data.repository.event.RepositoryCommitEvent;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Stream;
import org.junit.jupiter.api.Test;

class RepositoryKeyProviderTest {

  @Test
  void shouldUseCustomKeyProvider() {
    TestEntity entity = new TestEntity(123L);
    CollectionRepository<TestEntity> repo = new CollectionRepository<>(List.of(entity));

    repo.setKeyProvider(TestEntity::getId);

    assertEquals(123L, repo.getKey(entity));
  }

  @Test
  void shouldUseHasEntityKey() {
    EntityWithKey entity = new EntityWithKey(456L);
    CollectionRepository<EntityWithKey> repo = new CollectionRepository<>(List.of(entity));

    assertEquals(456L, repo.getKey(entity));
  }

  @Test
  void shouldPrioritizeCustomProviderOverHasEntityKey() {
    EntityWithKey entity = new EntityWithKey(456L);
    CollectionRepository<EntityWithKey> repo = new CollectionRepository<>(List.of(entity));

    repo.setKeyProvider(e -> 999L);

    assertEquals(999L, repo.getKey(entity));
  }

  @Test
  void shouldUseEntityItselfAsKeyByDefault() {
    TestEntity entity = new TestEntity(123L);
    CollectionRepository<TestEntity> repo = new CollectionRepository<>(List.of(entity));

    assertEquals(entity, repo.getKey(entity));
  }

  @Test
  void shouldNotQueryWhenSettingKeyProviderWithoutCommitListener() {
    List<RepositoryCriteria<TestEntity, Object>> queries = new ArrayList<>();
    DelegatingRepository<TestEntity, Object> repo = new DelegatingRepository<>(criteria -> {
      queries.add(criteria);
      return Stream.empty();
    }, criteria -> 0, key -> Optional.empty());

    repo.setKeyProvider(TestEntity::getId);

    assertEquals(List.of(), queries);
  }

  @Test
  void shouldNotifyCommitListenersWhenSettingKeyProvider() {
    TestEntity entity = new TestEntity(123L);
    CollectionRepository<TestEntity> repo = new CollectionRepository<>(List.of(entity));
    List<RepositoryCommitEvent<TestEntity>> events = new ArrayList<>();
    repo.onCommit(events::add);

    repo.setKeyProvider(TestEntity::getId);

    assertEquals(1, events.size());
    assertEquals(List.of(entity), events.get(0).getCommits());
  }

  @Test
  void shouldNotQueryWhenSettingKeyProviderAfterCommitListenerIsRemoved() {
    List<RepositoryCriteria<TestEntity, Object>> queries = new ArrayList<>();
    DelegatingRepository<TestEntity, Object> repo = new DelegatingRepository<>(criteria -> {
      queries.add(criteria);
      return Stream.empty();
    }, criteria -> 0, key -> Optional.empty());
    repo.onCommit(event -> {
    }).remove();

    repo.setKeyProvider(TestEntity::getId);

    assertEquals(List.of(), queries);
  }

  @Test
  void shouldNotQueryWhenCommitListenerDoesNotReadCommits() {
    List<RepositoryCriteria<TestEntity, Object>> queries = new ArrayList<>();
    DelegatingRepository<TestEntity, Object> repo = new DelegatingRepository<>(criteria -> {
      queries.add(criteria);
      return Stream.empty();
    }, criteria -> 0, key -> Optional.empty());
    repo.onCommit(event -> {
    });

    repo.setKeyProvider(TestEntity::getId);

    assertEquals(List.of(), queries);
  }

  @Test
  void shouldQueryOnceWhenCommitListenerReadsCommitsTwice() {
    List<RepositoryCriteria<TestEntity, Object>> queries = new ArrayList<>();
    DelegatingRepository<TestEntity, Object> repo = new DelegatingRepository<>(criteria -> {
      queries.add(criteria);
      return Stream.empty();
    }, criteria -> 0, key -> Optional.empty());
    repo.onCommit(event -> {
      event.getCommits().size();
      event.getCommits().size();
    });

    repo.setKeyProvider(TestEntity::getId);

    assertEquals(1, queries.size());
  }

  static class TestEntity {
    private final Long id;

    TestEntity(Long id) {
      this.id = id;
    }

    public Long getId() {
      return id;
    }
  }

  static class EntityWithKey implements HasEntityKey {
    private final Long id;

    EntityWithKey(Long id) {
      this.id = id;
    }

    @Override
    public Object getEntityKey() {
      return id;
    }
  }
}
