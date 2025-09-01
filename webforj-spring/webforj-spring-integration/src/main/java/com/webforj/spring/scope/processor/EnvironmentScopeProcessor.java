package com.webforj.spring.scope.processor;

import com.webforj.Environment;
import com.webforj.spring.scope.BeanStore;
import java.lang.System.Logger;
import java.lang.System.Logger.Level;
import org.springframework.beans.factory.ObjectFactory;
import org.springframework.beans.factory.config.Scope;

/**
 * Spring scope implementation that binds beans to the webforJ Environment lifecycle.
 *
 * <p>
 * This scope stores beans in the current Environment, which means they are created once per browser
 * window or tab and destroyed when the Environment is cleaned up (when the user closes the tab or
 * the session expires). Each browser window or tab has its own Environment with isolated bean
 * instances.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 25.03
 */
public class EnvironmentScopeProcessor implements Scope {
  private static final Logger logger = System.getLogger(EnvironmentScopeProcessor.class.getName());
  private static final String BEAN_STORE_KEY = "webforj.spring.scope.environment.store";

  /**
   * {@inheritDoc}
   */
  @Override
  public Object get(String name, ObjectFactory<?> objectFactory) {
    Environment env = getEnvironment();
    BeanStore store = BeanStore.getOrCreate(BEAN_STORE_KEY);
    String scopeId = getScopeId(env);
    return store.get(scopeId, name, objectFactory);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Object remove(String name) {
    Environment env = getEnvironmentOrNull();
    if (env == null) {
      return null;
    }

    BeanStore store = BeanStore.getOrCreate(BEAN_STORE_KEY);
    String scopeId = getScopeId(env);
    return store.remove(scopeId, name);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void registerDestructionCallback(String name, Runnable callback) {
    Environment env = getEnvironmentOrNull();
    if (env == null) {
      return;
    }

    BeanStore store = BeanStore.getOrCreate(BEAN_STORE_KEY);
    String scopeId = getScopeId(env);
    store.registerDestructionCallback(scopeId, name, callback);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Object resolveContextualObject(String key) {
    if ("webforj-environment".equals(key)) {
      return Environment.getCurrent();
    }

    return null;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getConversationId() {
    Environment env = getEnvironmentOrNull();
    if (env == null) {
      return null;
    }

    return getScopeId(env);
  }

  /**
   * Clean up all beans and callbacks for the current environment.
   */
  public static void cleanup() {
    Environment env = Environment.getCurrent();
    if (env == null) {
      return;
    }

    try {
      BeanStore.cleanupScopeInstance(BEAN_STORE_KEY, getScopeId(env));
    } catch (Exception e) {
      logger.log(Level.ERROR, "Error during environment scope cleanup", e);
    }
  }

  /**
   * Get the unique identifier for the given environment.
   *
   * @param env the environment
   * @return the scope identifier
   */
  private static String getScopeId(Environment env) {
    return "webforj-environment-" + System.identityHashCode(env);
  }

  /**
   * Gets the current environment or returns null if not available.
   *
   * @return the current environment or null
   */
  private Environment getEnvironmentOrNull() {
    return Environment.getCurrent();
  }

  /**
   * Gets the current environment or throws an exception if not available.
   *
   * @return the current environment
   * @throws IllegalStateException if no environment is available
   */
  private Environment getEnvironment() {
    Environment env = Environment.getCurrent();
    if (env == null) {
      throw new IllegalStateException(
          "No webforJ Environment available - webforj-environment scope is not active");
    }

    return env;
  }
}
