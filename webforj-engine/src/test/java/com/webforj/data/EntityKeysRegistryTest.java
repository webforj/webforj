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
}
