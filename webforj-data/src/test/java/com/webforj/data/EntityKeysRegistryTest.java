package com.webforj.data;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class EntityKeysRegistryTest {
  private EntityKeysRegistry registry;
  private Object testEntity;
  private String testEntityKey;

  @BeforeEach
  void setUp() {
    registry = new EntityKeysRegistry();
    testEntity = new Object();
    testEntityKey = registry.getKey(testEntity);
  }

  @Test
  void shouldReturnSameKeyForSameEntity() {
    String sameKey = registry.getKey(testEntity);
    assertEquals(testEntityKey, sameKey);
  }

  @Test
  void shouldReturnDifferentKeyForDifferentEntity() {
    Object differentEntity = new Object();
    String differentKey = registry.getKey(differentEntity);
    assertNotEquals(testEntityKey, differentKey);
  }

  @Test
  void shouldThrowNullPointerExceptionWhenEntityIsNull() {
    assertThrows(NullPointerException.class, () -> registry.getKey(null));
  }

  @Test
  void shouldReturnCorrectEntityForGivenKey() {
    Object sameEntity = registry.getEntity(testEntityKey);
    assertEquals(testEntity, sameEntity);
  }

  @Test
  void shouldReturnNullForNonExistentKey() {
    assertNull(registry.getEntity("nonexistent key"));
  }

  @Test
  void shouldThrowNullPointerExceptionWhenKeyIsNull() {
    assertThrows(NullPointerException.class, () -> registry.getEntity(null));
  }

  @Test
  void shouldReturnNewKeyForEntity() {
    String newKey = registry.refreshKey(testEntity);
    assertNotEquals(testEntityKey, newKey);
  }

  @Test
  void shouldRemoveOldKeyWhenRefreshingKey() {
    registry.refreshKey(testEntity);
    assertNull(registry.getEntity(testEntityKey));
  }

  @Test
  void shouldThrowNullPointerExceptionWhenEntityIsNullInRefreshKey() {
    assertThrows(NullPointerException.class, () -> registry.refreshKey(null));
  }

  @Test
  void shouldRemoveEntityAndItsKey() {
    registry.removeEntity(testEntity);
    assertNull(registry.getEntity(testEntityKey));
  }

  @Test
  void shouldCleanUpStaleEntries() {
    Object entity2 = new Object();
    String key2 = registry.getKey(entity2);
    registry.removeEntity(entity2); // Simulate stale entry
    registry.cleanUp();
    assertNull(registry.getEntity(key2));
  }

  @Test
  void shouldUseCustomKeyProviderWhenSet() {
    EntityKeysRegistry customRegistry = new EntityKeysRegistry();
    TestEntity entity = new TestEntity(123L);
    customRegistry.setKeyProvider(e -> ((TestEntity) e).getId());

    String key = customRegistry.getKey(entity);
    assertEquals("123", key);
  }

  @Test
  void shouldPrioritizeHasEntityKeyOverCustomProvider() {
    EntityKeysRegistry customRegistry = new EntityKeysRegistry();
    EntityWithKey entity = new EntityWithKey(456L);
    customRegistry.setKeyProvider(e -> 999L);

    String key = customRegistry.getKey(entity);
    assertEquals("456", key);
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
