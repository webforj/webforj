package com.webforj.data.repository;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.webforj.data.HasEntityKey;
import java.util.List;
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
