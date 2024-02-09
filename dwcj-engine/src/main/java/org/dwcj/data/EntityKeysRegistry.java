package org.dwcj.data;

import java.util.Objects;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;

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
public class EntityKeysRegistry {
  private final ConcurrentHashMap<Object, String> entityToKeyMap = new ConcurrentHashMap<>();
  private final ConcurrentHashMap<String, Object> keyToEntityMap = new ConcurrentHashMap<>();

  /**
   * Retrieves or generates a unique key for the specified entity. If the entity already has an
   * associated key, it returns the existing key. Otherwise, a new key is generated.
   *
   * <p>
   * If the entity implements {@link HasEntityKey}, the key provided by the entity is used. If not,
   * a new UUID (first 8 characters) is generated and used as the key. This method ensures that each
   * entity has a unique and consistent identifier.
   * </p>
   *
   * @param entity the entity for which a unique key is desired. Must not be null.
   *
   * @return the unique key associated with the entity.
   * @throws NullPointerException if the entity is null.
   */
  public String getKey(Object entity) {
    Objects.requireNonNull(entity, "Entity cannot be null");

    return entityToKeyMap.computeIfAbsent(entity, k -> {
      String id =
          (entity instanceof HasEntityKey) ? String.valueOf(((HasEntityKey) entity).getEntityKey())
              : UUID.randomUUID().toString().substring(0, 8);

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
    Objects.requireNonNull(entity, "Entity cannot be null");

    String oldKey = entityToKeyMap.remove(entity);
    if (oldKey != null) {
      keyToEntityMap.remove(oldKey);
    }

    return getKey(entity);
  }
}
