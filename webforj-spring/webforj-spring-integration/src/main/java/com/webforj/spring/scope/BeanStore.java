package com.webforj.spring.scope;

import com.webforj.environment.ObjectTable;
import com.webforj.environment.SessionObjectTable;
import java.lang.System.Logger;
import java.lang.System.Logger.Level;
import java.util.HashMap;
import java.util.Map;
import org.springframework.beans.factory.ObjectFactory;

/**
 * A store for managing scoped beans in an ObjectTable.
 *
 * <p>
 * The BeanStore provides a unified approach to storing and retrieving beans for a specific scope.
 * Each scope (environment, route, etc.) should have its own BeanStore instance. The store handles
 * bean creation, caching, and destruction callbacks.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 25.03
 */
public class BeanStore {
  /**
   * Defines the storage location for BeanStore instances.
   */
  public enum StorageType {
    /**
     * Store in ObjectTable (static/global storage).
     */
    LOCAL,

    /**
     * Store in SessionObjectTable (HTTP session storage).
     */
    SESSION
  }

  private static final String ERROR_DESTRUCTION_CALLBACK =
      "Error executing destruction callback for bean ''{0}'' in scope ''{1}''";
  private static final Logger logger = System.getLogger(BeanStore.class.getName());
  private final Map<String, Map<String, Object>> scopedBeans = new HashMap<>();
  private final Map<String, Map<String, Runnable>> destructionCallbacks = new HashMap<>();
  private static final LocalStorage LOCAL_STORAGE = new LocalStorage();
  private static final SessionStorage SESSION_STORAGE = new SessionStorage();

  BeanStore() {
    // no instantiation
  }

  /**
   * Gets or creates a BeanStore instance from the ObjectTable.
   *
   * @param objectTableKey the key to use in ObjectTable for storing the BeanStore
   * @return the BeanStore instance
   */
  public static BeanStore getOrCreate(String objectTableKey) {
    return getOrCreate(objectTableKey, StorageType.LOCAL);
  }

  /**
   * Gets or creates a BeanStore instance from the specified storage.
   *
   * @param key the key to use for storing the BeanStore
   * @param storageType the storage type to use
   *
   * @return the BeanStore instance
   */
  public static BeanStore getOrCreate(String key, StorageType storageType) {
    Storage storage = getStorage(storageType);
    if (!storage.contains(key)) {
      storage.put(key, new BeanStore());
    }

    return (BeanStore) storage.get(key);
  }

  /**
   * Cleans up a specific scope instance and removes the BeanStore from ObjectTable if empty.
   *
   * @param objectTableKey the key used in ObjectTable for the BeanStore
   * @param scopeId the scope instance to destroy
   *
   * @return true if the BeanStore was removed from ObjectTable
   */
  public static boolean cleanupScopeInstance(String objectTableKey, String scopeId) {
    return cleanupScopeInstance(objectTableKey, scopeId, StorageType.LOCAL);
  }

  /**
   * Cleans up a specific scope instance and removes the BeanStore from storage if empty.
   *
   * @param key the key used for the BeanStore
   * @param scopeId the scope instance to destroy
   * @param storageType the storage type to use
   *
   * @return true if the BeanStore was removed from storage
   */
  public static boolean cleanupScopeInstance(String key, String scopeId, StorageType storageType) {
    Storage storage = getStorage(storageType);
    if (!storage.contains(key)) {
      return false;
    }

    BeanStore store = (BeanStore) storage.get(key);
    store.destroyScopeInstance(scopeId);

    // Remove from storage if no more scopes
    if (store.getScopeCount() == 0) {
      storage.remove(key);
      logger.log(Level.DEBUG, "Removed BeanStore from {0} storage for key ''{1}''", storageType,
          key);
      return true;
    }

    return false;
  }

  /**
   * Cleans up all scope instances and removes the BeanStore from ObjectTable.
   *
   * @param objectTableKey the key used in ObjectTable for the BeanStore
   */
  public static void cleanupAllScopeInstances(String objectTableKey) {
    cleanupAllScopeInstances(objectTableKey, StorageType.LOCAL);
  }

  /**
   * Cleans up all scope instances and removes the BeanStore from storage.
   *
   * @param key the key used for the BeanStore
   * @param storageType the storage type to use
   */
  public static void cleanupAllScopeInstances(String key, StorageType storageType) {
    Storage storage = getStorage(storageType);
    if (!storage.contains(key)) {
      return;
    }

    BeanStore store = (BeanStore) storage.get(key);
    store.destroyAllScopeInstances();
    storage.remove(key);
    logger.log(Level.DEBUG, "Removed BeanStore from {0} storage for key ''{1}''", storageType, key);
  }

  /**
   * Retrieves a bean from the store, creating it if necessary.
   *
   * @param scopeId the identifier within the scope (e.g., environment ID, route class name)
   * @param beanName the name of the bean
   * @param objectFactory the factory to create the bean if it doesn't exist
   *
   * @return the bean instance
   */
  public Object get(String scopeId, String beanName, ObjectFactory<?> objectFactory) {
    Map<String, Object> beans = scopedBeans.computeIfAbsent(scopeId, k -> new HashMap<>());

    return beans.computeIfAbsent(beanName, k -> {
      Object bean = objectFactory.getObject();
      logger.log(Level.DEBUG, "Created bean ''{0}'' in scope ''{1}''", beanName, scopeId);
      return bean;
    });
  }


  /**
   * Removes a bean from the store and executes its destruction callback if present.
   *
   * @param scopeId the identifier within the scope
   * @param beanName the name of the bean to remove
   *
   * @return the removed bean, or null if not found
   */
  public Object remove(String scopeId, String beanName) {
    Map<String, Object> beans = scopedBeans.get(scopeId);
    if (beans == null) {
      return null;
    }
    Object removed = beans.remove(beanName);
    if (removed != null) {
      logger.log(Level.DEBUG, "Removed bean ''{0}'' from scope ''{1}''", beanName, scopeId);
      // Execute destruction callback if present
      executeDestructionCallback(scopeId, beanName);
    }
    return removed;
  }

  /**
   * Executes and removes a destruction callback for a specific bean.
   *
   * @param scopeId the identifier within the scope
   * @param beanName the name of the bean
   */
  private void executeDestructionCallback(String scopeId, String beanName) {
    Map<String, Runnable> callbacks = destructionCallbacks.get(scopeId);
    if (callbacks != null) {
      Runnable callback = callbacks.remove(beanName);
      if (callback != null) {
        try {
          callback.run();
          logger.log(Level.DEBUG, "Executed destruction callback for bean ''{0}'' in scope ''{1}''",
              beanName, scopeId);
        } catch (Exception e) {
          logger.log(Level.ERROR, ERROR_DESTRUCTION_CALLBACK, beanName, scopeId, e);
        }
      }
    }
  }

  /**
   * Registers a destruction callback for a bean.
   *
   * @param scopeId the identifier within the scope
   * @param beanName the name of the bean
   *
   * @param callback the callback to execute when the bean is destroyed
   */
  public void registerDestructionCallback(String scopeId, String beanName, Runnable callback) {
    Map<String, Runnable> callbacks =
        destructionCallbacks.computeIfAbsent(scopeId, k -> new HashMap<>());
    callbacks.put(beanName, callback);
    logger.log(Level.DEBUG, "Registered destruction callback for bean ''{0}'' in scope ''{1}''",
        beanName, scopeId);
  }

  /**
   * Removes a destruction callback for a bean.
   *
   * @param scopeId the identifier within the scope
   * @param beanName the name of the bean
   */
  public void removeDestructionCallback(String scopeId, String beanName) {
    Map<String, Runnable> callbacks = destructionCallbacks.get(scopeId);
    if (callbacks != null) {
      callbacks.remove(beanName);
    }
  }

  /**
   * Destroys all beans and executes destruction callbacks for a specific scope instance.
   *
   * @param scopeId the identifier within the scope
   */
  public void destroyScopeInstance(String scopeId) {
    // Execute destruction callbacks
    Map<String, Runnable> callbacks = destructionCallbacks.get(scopeId);
    if (callbacks != null) {
      logger.log(Level.DEBUG, "Destroying {0} beans in scope ''{1}''", callbacks.size(), scopeId);
      for (Map.Entry<String, Runnable> entry : callbacks.entrySet()) {
        try {
          entry.getValue().run();
        } catch (Exception e) {
          logger.log(Level.ERROR, ERROR_DESTRUCTION_CALLBACK, entry.getKey(), scopeId, e);
        }
      }
      destructionCallbacks.remove(scopeId);
    }

    // Clear beans
    Map<String, Object> beans = scopedBeans.remove(scopeId);
    if (beans != null && !beans.isEmpty()) {
      logger.log(Level.DEBUG, "Cleared {0} beans from scope ''{1}''", beans.size(), scopeId);
    }
  }

  /**
   * Destroys all beans and executes all destruction callbacks for all scope instances.
   */
  public void destroyAllScopeInstances() {
    int totalScopes = scopedBeans.size();
    if (totalScopes > 0) {
      logger.log(Level.DEBUG, "Destroying all beans in {0} scopes", totalScopes);
    }

    // Execute all destruction callbacks
    for (Map.Entry<String, Map<String, Runnable>> scopeEntry : destructionCallbacks.entrySet()) {
      String scopeId = scopeEntry.getKey();
      Map<String, Runnable> callbacks = scopeEntry.getValue();
      for (Map.Entry<String, Runnable> entry : callbacks.entrySet()) {
        try {
          entry.getValue().run();
        } catch (Exception e) {
          logger.log(Level.ERROR, ERROR_DESTRUCTION_CALLBACK, entry.getKey(), scopeId, e);
        }
      }
    }

    // Clear everything
    destructionCallbacks.clear();
    scopedBeans.clear();
  }


  /**
   * Checks if a specific scope instance has any beans.
   *
   * @param scopeId the identifier within the scope
   * @return true if the scope has beans, false otherwise
   */
  public boolean hasBeans(String scopeId) {
    Map<String, Object> beans = scopedBeans.get(scopeId);
    return beans != null && !beans.isEmpty();
  }

  /**
   * Gets the number of scope instances in this store.
   *
   * @return the number of scope instances
   */
  public int getScopeCount() {
    return scopedBeans.size();
  }

  /**
   * Gets the number of beans in a specific scope instance.
   *
   * @param scopeId the identifier within the scope
   * @return the number of beans, or 0 if the scope doesn't exist
   */
  public int getBeanCount(String scopeId) {
    Map<String, Object> beans = scopedBeans.get(scopeId);
    return beans != null ? beans.size() : 0;
  }

  /**
   * Gets the storage implementation for the given type.
   */
  private static Storage getStorage(StorageType type) {
    return type == StorageType.LOCAL ? LOCAL_STORAGE : SESSION_STORAGE;
  }

  /**
   * Interface that defines storage operations.
   */
  interface Storage {
    boolean contains(String key);

    Object get(String key);

    void put(String key, Object value);

    void remove(String key);
  }

  /**
   * ObjectTable Storage implementation.
   */
  static class LocalStorage implements Storage {
    @Override
    public boolean contains(String key) {
      return ObjectTable.contains(key);
    }

    @Override
    public Object get(String key) {
      return ObjectTable.get(key);
    }

    @Override
    public void put(String key, Object value) {
      ObjectTable.put(key, value);
    }

    @Override
    public void remove(String key) {
      ObjectTable.clear(key);
    }
  }

  /**
   * SessionObjectTable Storage implementation.
   */
  static class SessionStorage implements Storage {
    @Override
    public boolean contains(String key) {
      return SessionObjectTable.contains(key);
    }

    @Override
    public Object get(String key) {
      return SessionObjectTable.get(key);
    }

    @Override
    public void put(String key, Object value) {
      SessionObjectTable.put(key, value);
    }

    @Override
    public void remove(String key) {
      SessionObjectTable.clear(key);
    }
  }
}
