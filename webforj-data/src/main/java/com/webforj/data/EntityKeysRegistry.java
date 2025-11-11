package com.webforj.data;

import com.webforj.data.concern.HasKeyProvider;
import java.util.Objects;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Function;

/**
 * EntityKeysRegistry is a thread-safe mapping utility that associates unique string keys with
 * entities (objects). It is designed to generate and maintain a bi-directional relationship between
 * entities and their corresponding unique identifiers.
 *
 * <p>
 * This class is particularly useful in scenarios where entities need to be tracked or identified
 * uniquely across different layers of an application, such as sending entity identifiers to a
 * client-side component and then retrieving the original entity on the server-side based on that
 * identifier. Once an entity is registered with a key, the association is persistent throughout the
 * lifecycle of this registry instance.
 * </p>
 *
 * <p>
 * The registry supports custom key generation if the entity implements {@link HasEntityKey}
 * interface, providing flexibility in key management. For entities not implementing this interface,
 * a random UUID is used as the default key.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 24.00
 */
public class EntityKeysRegistry implements HasKeyProvider<Object> {
  private static final String ENTITY_CANNOT_BE_NULL = "Entity cannot be null";
  private final ConcurrentHashMap<Object, String> entityToKeyMap = new ConcurrentHashMap<>();
  private final ConcurrentHashMap<String, Object> keyToEntityMap = new ConcurrentHashMap<>();
  private Function<Object, ?> keyProvider;

  /**
   * {@inheritDoc}
   *
   * <h4>Priority Order for Key Generation:</h4>
   * <ol>
   * <li>{@link HasEntityKey} interface (if entity implements it)</li>
   * <li>Custom key provider (if set via this method)</li>
   * <li>Random UUID (fallback)</li>
   * </ol>
   */
  @Override
  public EntityKeysRegistry setKeyProvider(Function<Object, ?> keyProvider) {
    this.keyProvider = keyProvider;
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Function<Object, ?> getKeyProvider() {
    return keyProvider;
  }

  /**
   * Retrieves or generates a unique key for the specified entity. If the entity already has an
   * associated key, it returns the existing key. Otherwise, a new key is generated.
   *
   * <p>
   * Key generation follows this priority:
   * </p>
   * <ol>
   * <li>If entity implements {@link HasEntityKey}, use its key</li>
   * <li>If an identifier provider is set, use it to extract the key</li>
   * <li>Otherwise, generate a random UUID (first 8 characters)</li>
   * </ol>
   *
   * @param entity the entity for which a unique key is desired. Must not be null.
   *
   * @return the unique key associated with the entity.
   * @throws NullPointerException if the entity is null.
   */
  public String getKey(Object entity) {
    Objects.requireNonNull(entity, ENTITY_CANNOT_BE_NULL);

    return entityToKeyMap.computeIfAbsent(entity, k -> {
      String id;

      if (entity instanceof HasEntityKey hasEntityKey) {
        // Priority 1: HasEntityKey interface
        id = String.valueOf(hasEntityKey.getEntityKey());
      } else if (keyProvider != null) {
        // Priority 2: Custom key provider
        Object providedId = keyProvider.apply(entity);
        id = providedId != null ? String.valueOf(providedId) : null;
      } else {
        // Priority 3: Random UUID fallback
        id = null;
      }

      // If no ID could be determined, generate random UUID
      if (id == null) {
        id = UUID.randomUUID().toString().substring(0, 8);
      }

      keyToEntityMap.put(id, entity);
      return id;
    });
  }

  /**
   * Retrieves the entity associated with the specified key.
   *
   * <p>
   * This method looks up the entity in the registry using the provided key. It returns the entity
   * if found, or null if no association exists for the given key.
   * </p>
   *
   * @param key the key used to lookup the entity. Must not be null.
   *
   * @return the entity associated with the given key, or null if the key is not associated with any
   *         entity.
   * @throws NullPointerException if the key is null.
   */
  public Object getEntity(String key) {
    Objects.requireNonNull(key, "Key cannot be null");
    return keyToEntityMap.get(key);
  }

  /**
   * Refreshes the unique key for the specified entity.
   *
   * <p>
   * If the entity already has a key, it will be replaced with a new key. If the entity implements
   * {@link HasEntityKey}, the key provided by the entity is used. Otherwise, a new UUID (first 8
   * characters) is generated and used as the key.
   * </p>
   *
   * @param entity the entity for which to refresh the key. Must not be null.
   *
   * @return the new unique key associated with the entity.
   * @throws NullPointerException if the entity is null.
   */
  public String refreshKey(Object entity) {
    Objects.requireNonNull(entity, ENTITY_CANNOT_BE_NULL);

    String oldKey = entityToKeyMap.remove(entity);
    if (oldKey != null) {
      keyToEntityMap.remove(oldKey);
    }

    return getKey(entity);
  }

  /**
   * Removes the specified entity and its associated key from the registry.
   *
   * @param entity the entity to be removed. Must not be null.
   */
  public void removeEntity(Object entity) {
    Objects.requireNonNull(entity, ENTITY_CANNOT_BE_NULL);

    String key = entityToKeyMap.remove(entity);
    if (key != null) {
      keyToEntityMap.remove(key);
    }
  }

  /**
   * Cleans up the stale entries in the registry.
   */
  public void cleanUp() {
    keyToEntityMap.keySet().removeIf(key -> !entityToKeyMap.containsValue(key));
  }
}
