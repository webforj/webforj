package com.webforj.spring.scope;

import com.webforj.Environment;
import com.webforj.environment.ObjectTable;
import java.lang.System.Logger;
import java.lang.System.Logger.Level;
import java.util.HashMap;
import java.util.Map;
import org.springframework.beans.factory.ObjectFactory;
import org.springframework.beans.factory.config.Scope;

/**
 * Spring scope implementation that binds beans to the webforJ Environment lifecycle.
 *
 * <p>
 * This scope stores beans in the current Environment, which means they are created once per request
 * and destroyed when the Environment is cleaned up at the end of the request.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 25.03
 */
public class EnvironmentScopeProcessor implements Scope {
  private static final Logger logger = System.getLogger(EnvironmentScopeProcessor.class.getName());
  private static final String SCOPE_BEANS_KEY = "webforj.spring.scope.environment.beans";
  private static final String DESTRUCTION_CALLBACKS_KEY =
      "webforj.spring.scope.environment.callbacks";

  /**
   * {@inheritDoc}
   */
  @Override
  public Object get(String name, ObjectFactory<?> objectFactory) {
    Environment env = Environment.getCurrent();
    if (env == null) {
      throw new IllegalStateException(
          "No webforJ Environment available - " + "webforj-environment scope is not active");
    }

    Map<String, Object> beans = getBeans();
    Object bean = beans.get(name);
    if (bean == null) {
      bean = objectFactory.getObject();
      beans.put(name, bean);
      logger.log(Level.DEBUG, "Created bean ''{0}'' in webforj-environment scope", name);
    }

    return bean;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Object remove(String name) {
    Map<String, Object> beans = getBeans();
    Object removed = beans.remove(name);

    // Execute destruction callback if exists
    Map<String, Runnable> callbacks = getDestructionCallbacks();
    Runnable callback = callbacks.remove(name);
    if (callback != null) {
      try {
        callback.run();
      } catch (Exception e) {
        logger.log(Level.ERROR, "Error executing destruction callback for bean ''{0}''", name, e);
      }
    }

    if (removed != null) {
      logger.log(Level.DEBUG, "Removed bean ''{0}'' from webforj-environment scope", name);
    }

    return removed;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void registerDestructionCallback(String name, Runnable callback) {
    Map<String, Runnable> callbacks = getDestructionCallbacks();
    callbacks.put(name, callback);
    logger.log(Level.DEBUG, "Registered destruction callback for bean ''{0}''", name);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Object resolveContextualObject(String key) {
    if ("environment".equals(key)) {
      return Environment.getCurrent();
    }

    return null;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getConversationId() {
    Environment env = Environment.getCurrent();
    if (env == null) {
      return null;
    }

    // Use the Environment's hash code as a unique identifier for this request
    return "webforj-environment-" + System.identityHashCode(env);
  }

  /**
   * Get the bean storage map from ObjectTable.
   *
   * @return the bean storage map
   */
  @SuppressWarnings("unchecked")
  private Map<String, Object> getBeans() {
    if (!ObjectTable.contains(SCOPE_BEANS_KEY)) {
      ObjectTable.put(SCOPE_BEANS_KEY, new HashMap<String, Object>());
    }

    return (Map<String, Object>) ObjectTable.get(SCOPE_BEANS_KEY);
  }

  /**
   * Get the destruction callbacks map from ObjectTable.
   *
   * @return the destruction callbacks map
   */
  @SuppressWarnings("unchecked")
  private Map<String, Runnable> getDestructionCallbacks() {
    if (!ObjectTable.contains(DESTRUCTION_CALLBACKS_KEY)) {
      ObjectTable.put(DESTRUCTION_CALLBACKS_KEY, new HashMap<String, Runnable>());
    }

    return (Map<String, Runnable>) ObjectTable.get(DESTRUCTION_CALLBACKS_KEY);
  }

  /**
   * Clean up all beans and callbacks for the current environment. This should be called when the
   * Environment is being destroyed.
   */
  public static void cleanup() {
    if (!Environment.isPresent()) {
      return;
    }

    try {
      // Execute all destruction callbacks
      if (ObjectTable.contains(DESTRUCTION_CALLBACKS_KEY)) {
        @SuppressWarnings("unchecked")
        Map<String, Runnable> callbacks =
            (Map<String, Runnable>) ObjectTable.get(DESTRUCTION_CALLBACKS_KEY);
        for (Map.Entry<String, Runnable> entry : callbacks.entrySet()) {
          try {
            entry.getValue().run();
            logger.log(Level.DEBUG, "Executed destruction callback for bean ''{0}''",
                entry.getKey());
          } catch (Exception e) {
            logger.log(Level.ERROR, "Error executing destruction callback for bean ''{0}''",
                entry.getKey(), e);
          }
        }
      }

      // Clear the storage
      ObjectTable.clear(SCOPE_BEANS_KEY);
      ObjectTable.clear(DESTRUCTION_CALLBACKS_KEY);
      logger.log(Level.DEBUG, "Cleaned up webforj-environment scope");
    } catch (Exception e) {
      logger.log(Level.ERROR, "Error during environment scope cleanup", e);
    }
  }
}
