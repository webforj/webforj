package com.webforj.spring.scope.processor;

import com.webforj.spring.scope.BeanStore;
import com.webforj.spring.scope.BeanStore.StorageType;
import java.lang.System.Logger;
import java.lang.System.Logger.Level;
import org.springframework.beans.factory.ObjectFactory;
import org.springframework.beans.factory.config.Scope;

/**
 * Spring scope processor for webforJ session-scoped beans.
 *
 * @author Hyyan Abo Fakher
 * @since 25.03
 */
public class SessionScopeProcessor implements Scope {
  private static final Logger logger = System.getLogger(SessionScopeProcessor.class.getName());
  static final String BEAN_STORE_KEY = "webforj.spring.scope.session.store";
  static final String SCOPE_ID = "session";

  /**
   * {@inheritDoc}
   */
  @Override
  public Object get(String name, ObjectFactory<?> objectFactory) {
    BeanStore store = BeanStore.getOrCreate(BEAN_STORE_KEY, StorageType.SESSION);
    return store.get(SCOPE_ID, name, objectFactory);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Object remove(String name) {
    try {
      BeanStore store = BeanStore.getOrCreate(BEAN_STORE_KEY, StorageType.SESSION);
      return store.remove(SCOPE_ID, name);
    } catch (IllegalStateException e) {
      // Session not available
      return null;
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void registerDestructionCallback(String name, Runnable callback) {
    try {
      BeanStore store = BeanStore.getOrCreate(BEAN_STORE_KEY, StorageType.SESSION);
      store.registerDestructionCallback(SCOPE_ID, name, callback);
    } catch (IllegalStateException e) {
      // Session not available
      logger.log(Level.ERROR,
          "Could not register destruction callback - webforJ session not available");
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Object resolveContextualObject(String key) {
    return null;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getConversationId() {
    return SCOPE_ID;
  }

  /**
   * Clean up all beans and callbacks for the session.
   */
  public static void cleanup() {
    try {
      BeanStore.cleanupAllScopeInstances(BEAN_STORE_KEY, StorageType.SESSION);
    } catch (Exception e) {
      logger.log(Level.ERROR, "Error during webforJ session scope cleanup", e);
    }
  }
}
